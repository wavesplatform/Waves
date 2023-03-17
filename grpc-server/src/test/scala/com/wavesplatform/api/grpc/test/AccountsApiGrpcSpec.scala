package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{
  AccountRequest,
  AccountsApiGrpcImpl,
  BalanceResponse,
  BalancesRequest,
  DataEntryResponse,
  DataRequest,
  LeaseResponse
}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{DataTransactionData, Recipient}
import com.wavesplatform.state.{EmptyDataEntry, IntegerDataEntry}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global

import org.scalatest.BeforeAndAfterAll

class AccountsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair    = TxHelpers.signer(1)
  val recipient: KeyPair = TxHelpers.signer(2)

  "GetBalances should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val assetTransferAmount   = 123
    val wavesTransferAmount   = 456 + TestValues.fee
    val reverseTransferAmount = 1

    val issue     = TxHelpers.issue(sender)
    val transfer1 = TxHelpers.transfer(sender, recipient.toAddress, assetTransferAmount, issue.asset)
    val transfer2 = TxHelpers.transfer(sender, recipient.toAddress, wavesTransferAmount, Waves)
    val transfer3 = TxHelpers.transfer(recipient, sender.toAddress, reverseTransferAmount, Waves)

    d.appendBlock(issue)
    d.appendBlock(transfer1, transfer2, transfer3)

    d.liquidAndSolidAssert { () =>
      val expectedWavesBalance = wavesTransferAmount - TestValues.fee - reverseTransferAmount
      val expectedResult = List(
        BalanceResponse.of(
          BalanceResponse.Balance.Waves(BalanceResponse.WavesBalances(expectedWavesBalance, 0, expectedWavesBalance, expectedWavesBalance))
        ),
        BalanceResponse.of(BalanceResponse.Balance.Asset(Amount(ByteString.copyFrom(issue.asset.id.arr), assetTransferAmount)))
      )

      val (observer1, result1) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(ByteString.copyFrom(recipient.toAddress.bytes), Seq(ByteString.EMPTY, ByteString.copyFrom(issue.asset.id.arr))),
        observer1
      )
      result1.runSyncUnsafe() shouldBe expectedResult

      val (observer2, result2) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(ByteString.copyFrom(recipient.toAddress.bytes), Seq.empty),
        observer2
      )
      result2.runSyncUnsafe() shouldBe expectedResult
    }
  }

  "GetActiveLeases should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val lease1      = TxHelpers.lease(sender, recipient.toAddress, 123)
    val lease2      = TxHelpers.lease(sender, recipient.toAddress, 456)
    val lease3      = TxHelpers.lease(sender, recipient.toAddress, 789)
    val leaseCancel = TxHelpers.leaseCancel(lease1.id(), sender)

    d.appendBlock(lease1, lease2, lease3)
    d.appendBlock(leaseCancel)

    d.liquidAndSolidAssert { () =>
      val (observer, result) = createObserver[LeaseResponse]

      grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(recipient.toAddress.bytes)), observer)

      result.runSyncUnsafe() shouldBe List(
        LeaseResponse.of(
          ByteString.copyFrom(lease3.id().arr),
          ByteString.copyFrom(lease3.id().arr),
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(recipient.toAddress.publicKeyHash)))),
          lease3.amount.value,
          2
        ),
        LeaseResponse.of(
          ByteString.copyFrom(lease2.id().arr),
          ByteString.copyFrom(lease2.id().arr),
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(recipient.toAddress.publicKeyHash)))),
          lease2.amount.value,
          2
        )
      )
    }
  }

  "GetDataEntries should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val data       = TxHelpers.dataV2(sender, Seq(IntegerDataEntry("key1", 123), IntegerDataEntry("key2", 456), IntegerDataEntry("key3", 789)))
    val deleteData = TxHelpers.dataV2(sender, Seq(EmptyDataEntry("key1")))

    d.appendBlock(data)
    d.appendBlock(deleteData)

    d.liquidAndSolidAssert { () =>
      val (observer1, result1) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), "key2"), observer1)
      result1.runSyncUnsafe() shouldBe List(
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key2", DataTransactionData.DataEntry.Value.IntValue(456)))
        )
      )

      val (observer2, result2) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), ""), observer2)
      result2.runSyncUnsafe() shouldBe List(
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key2", DataTransactionData.DataEntry.Value.IntValue(456)))
        ),
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key3", DataTransactionData.DataEntry.Value.IntValue(789)))
        )
      )
    }
  }

  private def getGrpcApi(d: Domain) =
    new AccountsApiGrpcImpl(d.accountsApi)
}
