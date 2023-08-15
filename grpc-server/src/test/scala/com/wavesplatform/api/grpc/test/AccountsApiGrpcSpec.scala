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

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class AccountsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair              = TxHelpers.signer(1)
  val recipient: KeyPair           = TxHelpers.signer(2)
  val getBalanceSender: KeyPair    = TxHelpers.signer(3)
  val getBalanceRecipient: KeyPair = TxHelpers.signer(4)
  val leaseSender: KeyPair         = TxHelpers.signer(812)
  val leaseRecipient: KeyPair      = TxHelpers.signer(813)

  val timeout: FiniteDuration = 2.minutes

  "GetBalances should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(getBalanceSender)) { d =>
    val grpcApi = getGrpcApi(d)

    val assetTransferAmount       = 123
    val secondAssetTransferAmount = 939
    val wavesTransferAmount       = 456 + TestValues.fee + TestValues.fee
    val reverseTransferAmount     = 1

    val issue       = TxHelpers.issue(getBalanceSender)
    val secondIssue = TxHelpers.issue(getBalanceSender)
    val transfer1   = TxHelpers.transfer(getBalanceSender, getBalanceRecipient.toAddress, assetTransferAmount, issue.asset)
    val transfer2   = TxHelpers.transfer(getBalanceSender, getBalanceRecipient.toAddress, wavesTransferAmount, Waves)
    val transfer3   = TxHelpers.transfer(getBalanceRecipient, getBalanceSender.toAddress, reverseTransferAmount, Waves)
    val transfer4   = TxHelpers.transfer(getBalanceSender, getBalanceRecipient.toAddress, secondAssetTransferAmount, secondIssue.asset)

    d.appendBlock(issue, secondIssue)
    d.appendBlock(transfer1, transfer2, transfer3, transfer4)

    d.liquidAndSolidAssert { () =>
      val expectedWavesBalance = wavesTransferAmount - TestValues.fee - reverseTransferAmount
      val expectedResult = List(
        BalanceResponse.of(
          BalanceResponse.Balance.Waves(BalanceResponse.WavesBalances(expectedWavesBalance, 0, expectedWavesBalance, expectedWavesBalance))
        ),
        BalanceResponse.of(BalanceResponse.Balance.Asset(Amount(ByteString.copyFrom(issue.asset.id.arr), assetTransferAmount))),
        BalanceResponse.of(BalanceResponse.Balance.Asset(Amount(ByteString.copyFrom(secondIssue.asset.id.arr), secondAssetTransferAmount)))
      )

      val (observer1, result1) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(
          ByteString.copyFrom(getBalanceRecipient.toAddress.bytes),
          Seq(ByteString.EMPTY, ByteString.copyFrom(issue.asset.id.arr), ByteString.copyFrom(secondIssue.asset.id.arr))
        ),
        observer1
      )
      result1.runSyncUnsafe() shouldBe expectedResult

      val (observer2, result2) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(ByteString.copyFrom(getBalanceRecipient.toAddress.bytes), Seq.empty),
        observer2
      )
      result2.runSyncUnsafe() shouldBe expectedResult
    }
  }

  "GetScript" - {
    "Should work with easy DAPP script" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val scriptComplexity = 0
      val grpcApi          = getGrpcApi(d)

      val script = TxHelpers.script(
        s"""{-# STDLIB_VERSION 6 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |@Callable(i)
           |func foo(a: Int) = { ([], a == 42) }""".stripMargin
      )

      d.appendBlock(TxHelpers.setScript(sender, script))

      val r = Await.result(
        grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(sender.toAddress.bytes))),
        timeout
      )

      r.scriptBytes shouldBe ByteString.copyFrom(script.bytes().arr)
      r.complexity shouldBe scriptComplexity
      r.publicKey shouldBe ByteString.copyFrom(sender.publicKey.arr)
    }

    "Should work with hard EXPRESSION script" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val scriptComplexity = 609
      val grpcApi          = getGrpcApi(d)

      val script = TxHelpers.script(s"""
                                       |{-# STDLIB_VERSION 5 #-}
                                       |{-# CONTENT_TYPE EXPRESSION #-}
                                       |{-# SCRIPT_TYPE ACCOUNT #-}
                                       |
                                       |let alicePubKey  = base58'5AzfA9UfpWVYiwFwvdr77k6LWupSTGLb14b24oVdEpMM'
                                       |let bobPubKey    = base58'2KwU4vzdgPmKyf7q354H9kSyX9NZjNiq4qbnH2wi2VDF'
                                       |let cooperPubKey = base58'GbrUeGaBfmyFJjSQb9Z8uTCej5GzjXfRDVGJGrmgt5cD'
                                       |let stanPubKey  = base58'5AzfA9UfpWVYiwFw1dr77k6LWupSTGLb14b24oVdEpMM'
                                       |let barneyPubKey    = base58'2KwU4vzdgPmKyf7q354H9kSyX9NZjNiq4qbnH2wi2VDF'
                                       |let cossPubKey = base58'GbrUeGaBfmyFJjSQb9Z8uTCej5GzjXfRDVGJGrmgt5cD'
                                       |
                                       |let aliceSigned     = if(sigVerify(tx.bodyBytes, tx.proofs[0], alicePubKey)) then 1 else 0
                                       |let bobSigned       = if(sigVerify(tx.bodyBytes, tx.proofs[1], bobPubKey)) then 1 else 0
                                       |let stanSigned = if(sigVerify(tx.bodyBytes, tx.proofs[3], stanPubKey)) then 1 else 0
                                       |let barnySigned = if(sigVerify(tx.bodyBytes, tx.proofs[4], barneyPubKey)) then 1 else 0
                                       |let cossSigned = if(sigVerify(tx.bodyBytes, tx.proofs[4], cossPubKey)) then 1 else 0
                                       |let aSigned = if(sigVerify(tx.bodyBytes, tx.proofs[3], stanPubKey)) then 1 else 0
                                       |let bSigned = if(sigVerify(tx.bodyBytes, tx.proofs[4], barneyPubKey)) then 1 else 0
                                       |let cSigned = if(sigVerify(tx.bodyBytes, tx.proofs[4], cossPubKey)) then 1 else 0
                                       |func cooperSigned() = if(sigVerify(tx.bodyBytes, tx.proofs[2], cooperPubKey))then 1 else 0
                                       |
                                       |
                                       |let n = tx.bodyBytes
                                       |let a = sigVerify(tx.bodyBytes, tx.proofs[2], cooperPubKey)
                                       |let b = sigVerify(tx.bodyBytes, tx.proofs[4], stanPubKey)
                                       |let c = sigVerify(tx.bodyBytes, tx.proofs[3], stanPubKey)
                                       |
                                       |aliceSigned + bobSigned + cooperSigned() >= 2
                                       |""".stripMargin)

      d.appendBlock(TxHelpers.setScript(sender, script))

      val r = Await.result(
        grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(sender.toAddress.bytes))),
        timeout
      )

      r.scriptBytes shouldBe ByteString.copyFrom(script.bytes().arr)
      r.complexity shouldBe scriptComplexity
      r.publicKey shouldBe ByteString.copyFrom(sender.publicKey.arr)
    }

    "Should have error for invalid address" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val script = TxHelpers.script(
        s"""{-# STDLIB_VERSION 6 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |@Callable(i)
           |func foo(a: Int) = { ([], a == 42) }""".stripMargin
      )

      d.appendBlock(TxHelpers.setScript(sender, script))

      grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(new Array[Byte](28)))).value shouldBe None
    }

    "Should have error for address without script" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(sender.toAddress.bytes))).value shouldBe None
    }
  }

  "GetActiveLeases should work" - {
    "Request should return all active leases at the address" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(leaseSender)) { d =>
      val grpcApi = getGrpcApi(d)

      val lease1      = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 123)
      val lease2      = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 456)
      val lease3      = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 789)
      val leaseCancel = TxHelpers.leaseCancel(lease1.id(), leaseSender)

      d.appendBlock(lease1, lease2, lease3)
      d.appendBlock(leaseCancel)

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[LeaseResponse]

        grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)

        result.runSyncUnsafe() shouldBe List(
          LeaseResponse.of(
            ByteString.copyFrom(lease3.id().arr),
            ByteString.copyFrom(lease3.id().arr),
            ByteString.copyFrom(leaseSender.toAddress.bytes),
            Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(leaseRecipient.toAddress.publicKeyHash)))),
            lease3.amount.value,
            2
          ),
          LeaseResponse.of(
            ByteString.copyFrom(lease2.id().arr),
            ByteString.copyFrom(lease2.id().arr),
            ByteString.copyFrom(leaseSender.toAddress.bytes),
            Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(leaseRecipient.toAddress.publicKeyHash)))),
            lease2.amount.value,
            2
          )
        )
      }
    }

    "Request should return an empty response, for an address with all leases closed" in withDomain(
      DomainPresets.RideV6,
      AddrWithBalance.enoughBalances(leaseSender)
    ) { d =>
      val grpcApi      = getGrpcApi(d)
      val lease1       = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 123)
      val lease2       = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 912873)
      val leaseCansel1 = TxHelpers.leaseCancel(lease1.id(), leaseSender)
      val leaseCansel2 = TxHelpers.leaseCancel(lease2.id(), leaseSender)

      d.appendBlock(lease1, lease2)
      d.appendBlock(leaseCansel1, leaseCansel2)

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[LeaseResponse]
        grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)
        result.runSyncUnsafe() shouldBe List.empty
      }
    }

    "Request should return an empty response, for an address with no leases" in withDomain(
      DomainPresets.RideV6,
      AddrWithBalance.enoughBalances(leaseSender)
    ) { d =>
      val grpcApi            = getGrpcApi(d)
      val (observer, result) = createObserver[LeaseResponse]
      grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)
      result.runSyncUnsafe() shouldBe List.empty
    }

    "LeaseResponse should receive leases created via Invoke" in withDomain(
      DomainPresets.RideV6,
      AddrWithBalance.enoughBalances(leaseSender)
    ) { d =>
      val grpcApi = getGrpcApi(d)
      val leaseAmount = 4649
      val sc =
        s"""{-# STDLIB_VERSION 6 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |@Callable(i)
           |func lease()={
           |[
           | Lease(Address(base58'${leaseRecipient.toAddress}'), $leaseAmount)
           |]
           |}""".stripMargin

      println(sc)

      val script = TxHelpers.script(sc)
      d.appendBlock(TxHelpers.setScript(leaseSender, script), TxHelpers.invoke(leaseSender.toAddress, func = Option[String].apply("lease")))
      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[LeaseResponse]
        grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)

        result.runSyncUnsafe().head.amount shouldBe leaseAmount
      }
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
