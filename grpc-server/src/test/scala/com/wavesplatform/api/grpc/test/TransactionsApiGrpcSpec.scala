package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{ApplicationStatus, TransactionResponse, TransactionSnapshotResponse, TransactionSnapshotsRequest, TransactionsApiGrpcImpl, TransactionsRequest}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.protobuf.{ByteStrExt, PBSnapshots}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{StateSnapshot, TxMeta}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, BeforeAndAfterAll}

import scala.collection.immutable.VectorMap

class TransactionsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair    = TxHelpers.signer(1)
  val recipient: KeyPair = TxHelpers.signer(2)

  "GetTransactions should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val txsWithHeight = (1 to 3).flatMap { idx =>
      val txs = (1 to 10).map { _ =>
        TxHelpers.transfer(sender, recipient.toAddress, 1)
      }
      d.appendBlock(txs*)
      txs.map(_ -> (idx + 1))
    }

    val expectedTxs = txsWithHeight.reverse.map { case (tx, h) =>
      TransactionResponse.of(ByteString.copyFrom(tx.id().arr), h, Some(PBTransactions.protobuf(tx)), ApplicationStatus.SUCCEEDED, None)
    }

    d.liquidAndSolidAssert { () =>
      val (observer1, result1) = createObserver[TransactionResponse]
      grpcApi.getTransactions(
        TransactionsRequest.of(ByteString.copyFrom(sender.toAddress.bytes), None, Seq.empty),
        observer1
      )
      result1.runSyncUnsafe() shouldBe expectedTxs

      val (observer2, result2) = createObserver[TransactionResponse]
      grpcApi.getTransactions(
        TransactionsRequest.of(
          ByteString.EMPTY,
          Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(recipient.toAddress.publicKeyHash)))),
          Seq.empty
        ),
        observer2
      )
      result2.runSyncUnsafe() shouldBe expectedTxs

      val (observer3, result3) = createObserver[TransactionResponse]
      grpcApi.getTransactions(
        TransactionsRequest.of(ByteString.EMPTY, None, txsWithHeight.map { case (tx, _) => ByteString.copyFrom(tx.id().arr) }),
        observer3
      )
      result3.runSyncUnsafe() shouldBe expectedTxs.reverse
    }
  }

  "GetTransactionSnapshots" in withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(secondSigner)) { d =>
    val recipient = signer(2).toAddress
    val txs       = Seq.fill(5)(transfer(amount = 1, fee = 100_000, from = secondSigner, to = recipient))

    val firstThreeSnapshots = Seq(
      StateSnapshot(balances =
        VectorMap(
          (secondAddress, Waves)  -> (ENOUGH_AMT - 100_001),
          (recipient, Waves)      -> 1,
          (defaultAddress, Waves) -> 200_040_000 // reward and 40% fee
        )
      ),
      StateSnapshot(balances =
        VectorMap(
          (secondAddress, Waves)  -> (ENOUGH_AMT - 200_002),
          (recipient, Waves)      -> 2,
          (defaultAddress, Waves) -> 200_080_000
        )
      ),
      StateSnapshot(balances =
        VectorMap(
          (secondAddress, Waves)  -> (ENOUGH_AMT - 300_003),
          (recipient, Waves)      -> 3,
          (defaultAddress, Waves) -> 200_120_000
        )
      )
    )

    def getSnapshots() = {
      val request              = TransactionSnapshotsRequest.of(txs.map(_.id().toByteString))
      val (observer, response) = createObserver[TransactionSnapshotResponse]
      getGrpcApi(d).getTransactionSnapshots(request, observer)
      response.runSyncUnsafe().flatMap(_.snapshot).map(PBSnapshots.fromProtobuf(_, ByteStr.empty, 0)._1)
    }

    d.appendBlock(txs(0), txs(1))
    d.appendMicroBlock(txs(2))

    // both liquid and solid state
    getSnapshots() shouldBe firstThreeSnapshots

    // hardened state
    d.appendBlock(txs(3), txs(4))
    getSnapshots() shouldBe firstThreeSnapshots ++ Seq(
      StateSnapshot(balances =
        VectorMap(
          (secondAddress, Waves)  -> (ENOUGH_AMT - 400_004),
          (recipient, Waves)      -> 4,
          (defaultAddress, Waves) -> 400_340_000 // 2 blocks reward, 100% fee from previous block and 40% fee from current
        )
      ),
      StateSnapshot(balances =
        VectorMap(
          (secondAddress, Waves)  -> (ENOUGH_AMT - 500_005),
          (recipient, Waves)      -> 5,
          (defaultAddress, Waves) -> 400_380_000
        )
      )
    )
  }

  "NODE-973. GetTransactions should return correct data for orders with attachment" in {
    def checkOrderAttachment(txResponse: TransactionResponse, expectedAttachment: ByteStr): Assertion = {
      PBTransactions
        .vanilla(txResponse.getTransaction, unsafe = true)
        .explicitGet()
        .asInstanceOf[ExchangeTransaction]
        .order1
        .attachment shouldBe Some(expectedAttachment)
    }

    val matcher = TxHelpers.signer(1)
    val issuer  = TxHelpers.signer(2)
    val buyer   = TxHelpers.signer(3)
    withDomain(DomainPresets.TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(matcher, issuer, buyer)) { d =>
      val grpcApi = getGrpcApi(d)

      val attachment = ByteStr.fill(32)(1)
      val issue      = TxHelpers.issue(issuer)
      val exchange =
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(
            OrderType.BUY,
            Waves,
            issue.asset,
            amount = 2,
            version = Order.V4,
            sender = buyer,
            matcher = matcher,
            attachment = Some(attachment)
          ),
          TxHelpers.order(OrderType.SELL, Waves, issue.asset, amount = 2, version = Order.V4, sender = issuer, matcher = matcher),
          matcher = matcher,
          version = TxVersion.V3
        )

      d.appendBlock(issue)
      d.appendBlock(exchange)

      d.liquidAndSolidAssert { () =>
        val (observer1, result1) = createObserver[TransactionResponse]
        grpcApi.getTransactions(
          TransactionsRequest.of(ByteString.copyFrom(exchange.sender.toAddress.bytes), None, Seq.empty),
          observer1
        )

        checkOrderAttachment(result1.runSyncUnsafe().head, attachment)

        val (observer2, result2) = createObserver[TransactionResponse]
        grpcApi.getTransactions(
          TransactionsRequest.of(
            ByteString.EMPTY,
            Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(buyer.toAddress.publicKeyHash)))),
            Seq.empty
          ),
          observer2
        )

        checkOrderAttachment(result2.runSyncUnsafe().head, attachment)

        val (observer3, result3) = createObserver[TransactionResponse]
        grpcApi.getTransactions(
          TransactionsRequest.of(ByteString.EMPTY, None, Seq(ByteString.copyFrom(exchange.id().arr))),
          observer3
        )
        checkOrderAttachment(result3.runSyncUnsafe().head, attachment)
      }
    }
  }

  "NODE-922. GetTransactions should return elided transactions" in {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    val resender        = TxHelpers.signer(3)
    val recipient       = TxHelpers.signer(4)
    withDomain(
      TransactionStateSnapshot.configure(_.copy(lightNodeBlockFieldsAbsenceInterval = 0)),
      balances = AddrWithBalance.enoughBalances(sender)
    ) { d =>
      val grpcApi          = getGrpcApi(d)
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val resenderTxs = Seq(TxHelpers.transfer(resender, recipient.toAddress, 1.waves), TxHelpers.transfer(resender, recipient.toAddress, 2.waves))
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, resender.toAddress, 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        challengedBlockTx +: resenderTxs,
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      resenderTxs.foreach { tx =>
        d.transactionsApi.transactionById(tx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true
      }

      val expectedTxs = resenderTxs.reverse.map { tx =>
        TransactionResponse.of(ByteString.copyFrom(tx.id().arr), 1002, Some(PBTransactions.protobuf(tx)), ApplicationStatus.ELIDED, None)
      }

      d.liquidAndSolidAssert { () =>
        val (observer1, result1) = createObserver[TransactionResponse]
        grpcApi.getTransactions(
          TransactionsRequest.of(ByteString.copyFrom(resender.toAddress.bytes), None, Seq.empty),
          observer1
        )
        result1.runSyncUnsafe() shouldBe expectedTxs

        val (observer2, result2) = createObserver[TransactionResponse]
        grpcApi.getTransactions(
          TransactionsRequest.of(
            ByteString.EMPTY,
            Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(recipient.toAddress.publicKeyHash)))),
            Seq.empty
          ),
          observer2
        )
        result2.runSyncUnsafe() shouldBe expectedTxs

        val (observer3, result3) = createObserver[TransactionResponse]
        grpcApi.getTransactions(
          TransactionsRequest.of(ByteString.EMPTY, None, resenderTxs.map { tx => ByteString.copyFrom(tx.id().arr) }),
          observer3
        )
        result3.runSyncUnsafe() shouldBe expectedTxs.reverse
      }
    }
  }

  private def getGrpcApi(d: Domain) =
    new TransactionsApiGrpcImpl(d.blockchain, d.transactionsApi)
}
