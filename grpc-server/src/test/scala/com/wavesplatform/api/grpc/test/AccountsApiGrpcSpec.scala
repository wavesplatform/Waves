package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{AccountRequest, AccountsApiGrpcImpl, BalanceResponse, BalancesRequest, DataEntryResponse, DataRequest, LeaseResponse}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry.Value.BinaryValue
import com.wavesplatform.protobuf.transaction.{DataTransactionData, Recipient}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class AccountsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair = TxHelpers.signer(1)
  val recipient: KeyPair = TxHelpers.signer(2)
  val getBalanceSender: KeyPair = TxHelpers.signer(3)
  val getBalanceRecipient: KeyPair = TxHelpers.signer(4)
  val leaseSender: KeyPair = TxHelpers.signer(812)
  val leaseRecipient: KeyPair = TxHelpers.signer(813)

  val timeout: FiniteDuration = 2.minutes

  "GetBalances should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(getBalanceSender)) { d =>
    val grpcApi = getGrpcApi(d)

    val assetTransferAmount = 123
    val secondAssetTransferAmount = 939
    val wavesTransferAmount = 456 + TestValues.fee + TestValues.fee
    val reverseTransferAmount = 1

    val issue = TxHelpers.issue(getBalanceSender)
    val secondIssue = TxHelpers.issue(getBalanceSender)
    val transfer1 = TxHelpers.transfer(getBalanceSender, getBalanceRecipient.toAddress, assetTransferAmount, issue.asset)
    val transfer2 = TxHelpers.transfer(getBalanceSender, getBalanceRecipient.toAddress, wavesTransferAmount, Waves)
    val transfer3 = TxHelpers.transfer(getBalanceRecipient, getBalanceSender.toAddress, reverseTransferAmount, Waves)
    val transfer4 = TxHelpers.transfer(getBalanceSender, getBalanceRecipient.toAddress, secondAssetTransferAmount, secondIssue.asset)

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
    "Should work with  DAPP script" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val scriptComplexity = 2
      val grpcApi = getGrpcApi(d)

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
//      r.complexity shouldBe scriptComplexity
    //  r.scriptText shouldBe "DApp(DAppMeta(2,List(CallableFuncSignature(<ByteString@4254de26 size=1 contents=\"\\001\">,UnknownFieldSet(Map()))),List(),List(),UnknownFieldSet(Map())),List(),List(CallableFunction(CallableAnnotation(i),FUNC(call,List(a),FUNCTION_CALL(Native(1300),List(REF(nil), FUNCTION_CALL(Native(0),List(REF(a), 42))))))),None)"
    //    r.publicKey shouldBe ByteString.copyFrom(sender.publicKey.arr)

    }

    "NODE-957. Should work with max complexity expression script" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val scriptComplexity = 609
      val grpcApi = getGrpcApi(d)

      val script = TxHelpers.script(
        s"""
           |{-# STDLIB_VERSION 6 #-}
           |{-# CONTENT_TYPE EXPRESSION #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |${"sigVerify(base58'', base58'', base58'') && " * 10} true
           |""".stripMargin)
      print(script)
      d.appendBlock(TxHelpers.setScript(sender, script))

      val r = Await.result(
        grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(sender.toAddress.bytes))),
        timeout
      )

      r.scriptBytes shouldBe ByteString.copyFrom(script.bytes().arr)
     // r.complexity shouldBe scriptComplexity
      //  r.publicKey shouldBe ByteString.copyFrom(sender.publicKey.arr)
    }

//todo invalid address error
    "Should throw error for invalid address" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
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

    "NODE-974. Should return emtpy response for address without script" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(sender.toAddress.bytes))).value shouldBe None
    }
  }

  "GetActiveLeases should work" - {
    "NODE-938. Request should return all active leases at the address" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(leaseSender)) { d =>
      val grpcApi = getGrpcApi(d)

      val lease1 = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 123)
      val lease2 = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 456)
      val lease3 = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 789)
      val leaseCancel = TxHelpers.leaseCancel(lease1.id(), leaseSender)

      d.appendBlock(lease1, lease2, lease3, leaseCancel)

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
            height = 2
          ),
          LeaseResponse.of(
            ByteString.copyFrom(lease2.id().arr),
            ByteString.copyFrom(lease2.id().arr),
            ByteString.copyFrom(leaseSender.toAddress.bytes),
            Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(leaseRecipient.toAddress.publicKeyHash)))),
            lease2.amount.value,
            height = 2
          )
        )
      }
    }

    "NODE-960. Request should return an empty response, for an address with all canceled leases" in withDomain(
      DomainPresets.RideV6,
      AddrWithBalance.enoughBalances(leaseSender)
    ) { d =>
      val grpcApi = getGrpcApi(d)
      val lease = TxHelpers.lease(leaseSender, leaseRecipient.toAddress, 123)
      val leaseCansel = TxHelpers.leaseCancel(lease.id(), leaseSender)

      d.appendBlock(lease, leaseCansel)

      d.liquidAndSolidAssert { () =>
        val (observer, result) = createObserver[LeaseResponse]
        grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)
        result.runSyncUnsafe() shouldBe List.empty
      }
    }

    "NODE-959. Request should return an empty response, for an address with no leases" in withDomain(
      DomainPresets.RideV6,
      AddrWithBalance.enoughBalances(leaseSender)
    ) { d =>
      val grpcApi = getGrpcApi(d)
      val (observer, result) = createObserver[LeaseResponse]
      grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)
      result.runSyncUnsafe() shouldBe List.empty
    }

    "NODE-1128. LeaseResponse should receive leases created via Invoke" in withDomain(
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
           | let lease =  Lease(Address(base58'${leaseRecipient.toAddress}'), $leaseAmount)
           | let leaseId = calculateLeaseId(lease)
           | [
           |   lease,
           |   BinaryEntry("leaseId", leaseId)
           | ]
           |}""".stripMargin

      val script = TxHelpers.script(sc)
      val setScriptTx = TxHelpers.setScript(leaseSender, script)
      d.appendBlock(
        setScriptTx,
        TxHelpers.invoke(leaseSender.toAddress, func = Option[String].apply("lease"))
      )
      d.liquidAndSolidAssert { () =>
        val (dataObserver, dataResult) = createObserver[DataEntryResponse]
        grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(leaseSender.toAddress.bytes), "leaseId"), dataObserver)
        val leaseId = dataResult.runSyncUnsafe().head.entry.get.value.asInstanceOf[BinaryValue]

        val (observer, result) = createObserver[LeaseResponse]
        grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(leaseRecipient.toAddress.bytes)), observer)

        LeaseResponse.of(
          ByteString.copyFrom(setScriptTx.id().arr),
          leaseId.value,
          ByteString.copyFrom(leaseSender.toAddress.bytes),
          Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(leaseRecipient.toAddress.publicKeyHash)))),
          leaseAmount,
          height = 2
        )

        result.runSyncUnsafe().head.amount shouldBe leaseAmount
      }
    }
  }

  "NODE-976. GetDataEntries should not show deleted and non existing entries" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val data = TxHelpers.dataV2(sender, Seq(
      IntegerDataEntry("deleted_key", 123)
    ))

    val deleteData = TxHelpers.dataV2(sender, Seq(EmptyDataEntry("deleted_key")))

    d.appendBlock(data)
    d.appendBlock(deleteData)

    d.liquidAndSolidAssert { () =>
      val (observer1, result1) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), "non_existing_key"), observer1)
      result1.runSyncUnsafe() shouldBe List.empty

      val (observer2, result2) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), "deleted_key"), observer2)
      result2.runSyncUnsafe() shouldBe List.empty


      val (observer3, result3) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), ""), observer3)
      result3.runSyncUnsafe() shouldBe List.empty
    }

  }

  "NODE-975. GetDataEntries should return sorted data for empty key" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val data = TxHelpers.dataV2(sender, Seq(
      IntegerDataEntry("key_int", 456),
      StringDataEntry("key_str", "some str"),
      BooleanDataEntry("key_bool_true", true),
      BooleanDataEntry("key_bool_false", false),
      BinaryDataEntry("key_bin", ByteStr("test".getBytes()))
    ))

    d.appendBlock(data)

    d.liquidAndSolidAssert { () =>

      val (observer, result) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), ""), observer)
      val runResult = result.runSyncUnsafe()
      runResult shouldBe List(
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key_int", DataTransactionData.DataEntry.Value.IntValue(456)))
        ),
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key_str", DataTransactionData.DataEntry.Value.StringValue("some str")))
        ),
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key_bool_true", DataTransactionData.DataEntry.Value.BoolValue(true)))
        ),
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key_bool_false", DataTransactionData.DataEntry.Value.BoolValue(false)))
        ),
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataTransactionData.DataEntry.of("key_bin", DataTransactionData.DataEntry.Value.BinaryValue(ByteString.copyFrom("test".getBytes()))))
        )
      ).sortBy(_.entry.get.key)
    }
  }

  "NODE-989. Resolve alias should address for existing alias" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)
    val aliasName = "valid-alias"
    val alias = TxHelpers.createAlias(aliasName, sender)

    d.appendBlock(alias)
    d.liquidAndSolidAssert { () =>
      val aliasResponse = Await.result(
        grpcApi.resolveAlias(aliasName),
        timeout
      )
      aliasResponse shouldBe ByteString.copyFrom(sender.toAddress.bytes)
    }
  }

  "NODE-991. ResolveAlias for invalid alias should throw error" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    intercept[Exception](Await.result(grpcApi.resolveAlias("NonValidAlias"), timeout)).toString should include("Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz")
  }

  "NODE-1127. ResolveAlias for non exiting alias should" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    intercept[Exception](Await.result(grpcApi.resolveAlias("nonexisting"), timeout)).toString should include("nonexisting' doesn't exist")
  }

  private def getGrpcApi(d: Domain) =
    new AccountsApiGrpcImpl(d.accountsApi)
}
