package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{ApplicationStatus, TransactionResponse, TransactionsApiGrpcImpl, TransactionsRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, BeforeAndAfterAll}

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
          TxHelpers.order(OrderType.BUY, Waves, issue.asset, version = Order.V4, sender = buyer, matcher = matcher, attachment = Some(attachment)),
          TxHelpers.order(OrderType.SELL, Waves, issue.asset, version = Order.V4, sender = issuer, matcher = matcher),
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

  private def getGrpcApi(d: Domain) =
    new TransactionsApiGrpcImpl(d.blockchain, d.transactionsApi)
}
