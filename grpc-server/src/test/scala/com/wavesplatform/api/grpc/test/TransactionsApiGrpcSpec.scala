package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{ApplicationStatus, TransactionResponse, TransactionsApiGrpcImpl, TransactionsRequest}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.test.FreeSpec
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

  private def getGrpcApi(d: Domain) =
    new TransactionsApiGrpcImpl(d.blockchain, d.transactionsApi)
}
