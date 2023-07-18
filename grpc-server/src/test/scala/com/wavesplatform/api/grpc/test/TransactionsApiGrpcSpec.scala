package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{ApplicationStatus, TransactionResponse, TransactionsApiGrpcImpl, TransactionsRequest}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.state.TxMeta
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll

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

  "NODE-922. GetTransactions should return elided transactions" in {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    val resender        = TxHelpers.signer(3)
    val recipient       = TxHelpers.signer(4)
    withDomain(DomainPresets.TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi          = getGrpcApi(d)
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val resenderTxs = Seq(TxHelpers.transfer(resender, recipient.toAddress, 1.waves), TxHelpers.transfer(resender, recipient.toAddress, 2.waves))
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, resender.toAddress, 1005.waves)
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
