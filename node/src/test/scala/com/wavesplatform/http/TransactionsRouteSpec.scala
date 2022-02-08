package com.wavesplatform.http

import scala.concurrent.Future
import scala.util.Random

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import com.wavesplatform.{BlockGen, TestTime, TestValues, TestWallet}
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, _}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.{BlockchainFeatures => BF}
import com.wavesplatform.history.{settingsWithFeatures, Domain}
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.LeaseCancel
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.{Blockchain, Height, InvokeScriptResult, TxMeta}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.test._
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, GenesisTransaction, Proofs, TxHelpers, TxVersion}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, TracedResult}
import monix.reactive.Observable
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper

class TransactionsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with BlockGen
    with OptionValues
    with TestWallet
    with WithDomain {

  private val blockchain          = mock[Blockchain]
  private val utxPoolSynchronizer = mock[TransactionPublisher]
  private val addressTransactions = mock[CommonTransactionsApi]
  private val utxPoolSize         = mockFunction[Int]
  private val testTime            = new TestTime

  private val transactionsApiRoute = new TransactionsApiRoute(
    restAPISettings,
    addressTransactions,
    testWallet,
    blockchain,
    utxPoolSize,
    utxPoolSynchronizer,
    testTime
  )

  private val route = seal(transactionsApiRoute.route)

  private val invalidBase58Gen = alphaNumStr.map(_ + "0")

  routePath("/calculateFee") - {
    "waves" in {
      val transferTx = Json.obj(
        "type"            -> 4,
        "version"         -> 1,
        "amount"          -> 1000000,
        "feeAssetId"      -> JsNull,
        "senderPublicKey" -> TestValues.keyPair.publicKey,
        "recipient"       -> TestValues.address
      )
      (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 100000L, 0L))).once()

      Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
        (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
      }
    }

    "asset" in {
      val asset: IssuedAsset = TestValues.asset
      val transferTx = Json.obj(
        "type"            -> 4,
        "version"         -> 2,
        "amount"          -> 1000000,
        "feeAssetId"      -> asset.id.toString,
        "senderPublicKey" -> TestValues.keyPair.publicKey,
        "recipient"       -> TestValues.address
      )

      (addressTransactions.calculateFee _).expects(*).returning(Right((asset, 5L, 0L))).once()

      Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe asset.id.toString
        (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 5
      }
    }
  }

  private def mkRoute(d: Domain): Route =
    seal(
      new TransactionsApiRoute(
        restAPISettings,
        d.commonApi.transactions,
        testWallet,
        d.blockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        ntpTime
      ).route
    )

  "returns lease details for lease cancel transaction" in withDomain(settingsWithFeatures(BF.SmartAccounts)) { d =>
    val sender      = testWallet.generateNewAccount().get
    val recipient   = testWallet.generateNewAccount().get
    val lease       = LeaseTransaction.selfSigned(2.toByte, sender, recipient.toAddress, 5.waves, 0.001.waves, ntpTime.getTimestamp()).explicitGet()
    val leaseCancel = LeaseCancelTransaction.selfSigned(2.toByte, sender, lease.id(), 0.001.waves, ntpTime.getTimestamp()).explicitGet()
    val sealedRoute = mkRoute(d)

    d.appendBlock(
      GenesisTransaction.create(sender.toAddress, 10.waves, ntpTime.getTimestamp()).explicitGet(),
      GenesisTransaction.create(recipient.toAddress, 10.waves, ntpTime.getTimestamp()).explicitGet(),
      lease
    )

    def expectedJson(status: String, cancelHeight: Option[Int] = None, cancelTransactionId: Option[ByteStr] = None): JsObject =
      Json.parse(s"""{
         |  "type" : 9,
         |  "id" : "${leaseCancel.id()}",
         |  "sender" : "${sender.toAddress}",
         |  "senderPublicKey" : "${sender.publicKey}",
         |  "fee" : ${0.001.waves},
         |  "feeAssetId" : null,
         |  "timestamp" : ${leaseCancel.timestamp},
         |  "proofs" : [ "${leaseCancel.signature}" ],
         |  "version" : 2,
         |  "leaseId" : "${lease.id()}",
         |  "chainId" : 84,
         |  "spentComplexity" : 0,
         |  "lease" : {
         |    "id" : "${lease.id()}",
         |    "originTransactionId" : "${lease.id()}",
         |    "sender" : "${sender.toAddress}",
         |    "recipient" : "${recipient.toAddress}",
         |    "amount" : ${5.waves},
         |    "height" : 1,
         |    "status" : "$status",
         |    "cancelHeight" : ${cancelHeight.getOrElse("null")},
         |    "cancelTransactionId" : ${cancelTransactionId.fold("null")("\"" + _ + "\"")}
         |  }
         |}""".stripMargin).as[JsObject]

    d.utxPool.putIfNew(leaseCancel)

    withClue(routePath("/unconfirmed")) {
      Get(routePath(s"/unconfirmed")) ~> sealedRoute ~> check {
        responseAs[Seq[JsObject]].head should matchJson(expectedJson("active") - "spentComplexity")
      }
    }

    d.appendBlock(leaseCancel)

    val cancelTransactionJson = expectedJson("canceled", Some(2), Some(leaseCancel.id())) ++ Json.obj("height" -> 2)

    withClue(routePath("/address/{address}/limit/{limit}")) {
      Get(routePath(s"/address/${recipient.toAddress}/limit/10")) ~> sealedRoute ~> check {
        val json = (responseAs[JsArray] \ 0 \ 0).as[JsObject]
        json should matchJson(cancelTransactionJson)
      }
    }

    withClue(routePath("/info/{id}")) {
      Get(routePath(s"/info/${leaseCancel.id()}")) ~> sealedRoute ~> check {
        responseAs[JsObject] should matchJson(cancelTransactionJson)
      }
    }
  }

  routePath("/address/{address}/limit/{limit}") - {
    val bytes32StrGen = bytes32gen.map(Base58.encode)
    val addressGen    = accountGen.map(_.toAddress.toString)

    "handles parameter errors with corresponding responses" - {
      "invalid address" in {
        forAll(bytes32StrGen) { badAddress =>
          Get(routePath(s"/address/$badAddress/limit/1")) ~> route should produce(InvalidAddress)
        }
      }

      "invalid limit" - {
        "limit is too big" in {
          forAll(addressGen, choose(MaxTransactionsPerRequest + 1, Int.MaxValue).label("limitExceeded")) {
            case (address, limit) =>
              Get(routePath(s"/address/$address/limit/$limit")) ~> route should produce(TooBigArrayAllocation)
          }
        }
      }

      "invalid after" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), invalidBase58Gen) {
          case (address, limit, invalidBase58) =>
            Get(routePath(s"/address/$address/limit/$limit?after=$invalidBase58")) ~> route ~> check {
              status shouldEqual StatusCodes.BadRequest
              (responseAs[JsObject] \ "message").as[String] shouldEqual s"Unable to decode transaction id $invalidBase58"
            }
        }
      }
    }

    "returns 200 if correct params provided" - {
      "address and limit" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect")) {
          case (address, limit) =>
            (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once()
            (addressTransactions.transactionsByAddress _).expects(*, *, *, None).returning(Observable.empty).once()
            Get(routePath(s"/address/$address/limit/$limit")) ~> route ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }

      "address, limit and after" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), bytes32StrGen) {
          case (address, limit, txId) =>
            (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once()
            (addressTransactions.transactionsByAddress _).expects(*, *, *, *).returning(Observable.empty).once()
            Get(routePath(s"/address/$address/limit/$limit?after=$txId")) ~> route ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }
    }

    "provides stateChanges" in forAll(accountGen) { account =>
      val transaction = TxHelpers.invoke(account.toAddress, "test")

      (() => blockchain.activatedFeatures).expects().returns(Map.empty).anyNumberOfTimes()
      (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once()
      (addressTransactions.transactionsByAddress _)
        .expects(account.toAddress, *, *, None)
        .returning(Observable(TransactionMeta.Invoke(Height(1), transaction, succeeded = true, 0L, Some(InvokeScriptResult()))))
        .once()

      Get(routePath(s"/address/${account.toAddress}/limit/1")) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        (responseAs[JsArray] \ 0 \ 0 \ "stateChanges").as[JsObject] shouldBe Json.toJsObject(InvokeScriptResult())
      }
    }

    "provides lease and lease cancel actions stateChanges" in {
      val invokeAddress    = accountGen.sample.get.toAddress
      val leaseId1         = ByteStr(bytes32gen.sample.get)
      val leaseId2         = ByteStr(bytes32gen.sample.get)
      val leaseCancelId    = ByteStr(bytes32gen.sample.get)
      val recipientAddress = accountGen.sample.get.toAddress
      val recipientAlias   = aliasGen.sample.get
      val invoke           = TxHelpers.invoke(invokeAddress, "test")
      val scriptResult = InvokeScriptResult(
        leases = Seq(InvokeScriptResult.Lease(recipientAddress, 100, 1, leaseId1), InvokeScriptResult.Lease(recipientAlias, 200, 3, leaseId2)),
        leaseCancels = Seq(LeaseCancel(leaseCancelId))
      )

      (blockchain.leaseDetails _)
        .expects(leaseId1)
        .returning(Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Active, leaseId1, 1)))
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(leaseId2)
        .returning(Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Active, leaseId2, 1)))
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(leaseCancelId)
        .returning(
          Some(
            LeaseDetails(
              TestValues.keyPair.publicKey,
              TestValues.address,
              123,
              LeaseDetails.Status.Cancelled(2, Some(leaseCancelId)),
              leaseCancelId,
              1
            )
          )
        )
        .anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseId1).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseId2).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseCancelId).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()

      (() => blockchain.activatedFeatures).expects().returning(Map.empty).anyNumberOfTimes()
      (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once()
      (addressTransactions.transactionsByAddress _)
        .expects(invokeAddress, *, *, None)
        .returning(Observable(TransactionMeta.Invoke(Height(1), invoke, succeeded = true, 0L, Some(scriptResult))))
        .once()

      Get(routePath(s"/address/${invokeAddress}/limit/1")) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        val json = (responseAs[JsArray] \ 0 \ 0 \ "stateChanges").as[JsObject]
        json should matchJson(s"""{
                                    |  "data": [],
                                    |  "transfers": [],
                                    |  "issues": [],
                                    |  "reissues": [],
                                    |  "burns": [],
                                    |  "sponsorFees": [],
                                    |  "leases": [
                                    |    {
                                    |      "id": "$leaseId1",
                                    |      "originTransactionId": "$leaseId1",
                                    |      "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "recipient": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "amount": 123,
                                    |      "height": 1,
                                    |      "status":"active",
                                    |      "cancelHeight" : null,
                                    |      "cancelTransactionId" : null
                                    |    },
                                    |    {
                                    |      "id": "$leaseId2",
                                    |      "originTransactionId": "$leaseId2",
                                    |      "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "recipient": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "amount": 123,
                                    |      "height": 1,
                                    |      "status":"active",
                                    |      "cancelHeight" : null,
                                    |      "cancelTransactionId" : null
                                    |    }
                                    |  ],
                                    |  "leaseCancels": [
                                    |    {
                                    |      "id": "$leaseCancelId",
                                    |      "originTransactionId": "$leaseCancelId",
                                    |      "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "recipient": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "amount": 123,
                                    |      "height": 1,
                                    |      "status":"canceled",
                                    |      "cancelHeight" : 2,
                                    |      "cancelTransactionId" : "$leaseCancelId"
                                    |    }
                                    |  ],
                                    |  "invokes": []
                                    |}""".stripMargin)
      }
    }
  }

  routePath("/info/{id}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidTransactionId("Wrong char"), matchMsg = true)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidTransactionId("Transaction ID was not specified"))
      Get(routePath(s"/info")) ~> route should produce(InvalidTransactionId("Transaction ID was not specified"))
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx                           <- randomTransactionGen
        height                       <- posNum[Int]
        acceptFailedActivationHeight <- posNum[Int]
        succeed                      <- if (height >= acceptFailedActivationHeight) Arbitrary.arbBool.arbitrary else Gen.const(true)
      } yield (tx, succeed, height, acceptFailedActivationHeight)

      forAll(txAvailability) {
        case (tx, succeed, height, acceptFailedActivationHeight) =>
          (addressTransactions.transactionById _).expects(tx.id()).returning(Some(TransactionMeta.Default(Height(height), tx, succeed, 0L))).once()
          (() => blockchain.activatedFeatures)
            .expects()
            .returning(Map(BF.BlockV5.id -> acceptFailedActivationHeight))
            .anyNumberOfTimes()

          def validateResponse(): Unit = {
            status shouldEqual StatusCodes.OK

            val extraFields = Seq(
              (if (blockchain.isFeatureActivated(BF.BlockV5, height))
                 Json.obj("applicationStatus" -> JsString(if (succeed) "succeeded" else "script_execution_failed"))
               else Json.obj()),
              Json.obj("height" -> height, "spentComplexity" -> 0)
            ).reduce(_ ++ _)

            responseAs[JsValue] should matchJson(tx.json() ++ extraFields)
          }

          Get(routePath(s"/info/${tx.id().toString}")) ~> route ~> check(validateResponse())
      }
    }

    "provides stateChanges" in forAll(accountGen) { account =>
      val transaction = TxHelpers.invoke(account.toAddress, "test")

      (() => blockchain.activatedFeatures).expects().returns(Map.empty).anyNumberOfTimes()
      (addressTransactions.transactionById _)
        .expects(transaction.id())
        .returning(Some(TransactionMeta.Invoke(Height(1), transaction, succeeded = true, 0L, Some(InvokeScriptResult()))))
        .once()

      Get(routePath(s"/info/${transaction.id()}")) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        (responseAs[JsObject] \ "stateChanges").as[JsObject] shouldBe Json.toJsObject(InvokeScriptResult())
      }
    }

    "provides lease and lease cancel action stateChanges" in {
      val invokeAddress    = accountGen.sample.get.toAddress
      val recipientAddress = accountGen.sample.get.toAddress
      val recipientAlias   = aliasGen.sample.get

      val leaseId1      = ByteStr(bytes32gen.sample.get)
      val leaseId2      = ByteStr(bytes32gen.sample.get)
      val leaseCancelId = ByteStr(bytes32gen.sample.get)

      val nestedInvokeAddress = accountGen.sample.get.toAddress
      val nestedLeaseId       = ByteStr(bytes32gen.sample.get)
      val nestedLeaseCancelId = ByteStr(bytes32gen.sample.get)

      val invoke = TxHelpers.invoke(invokeAddress, "test")
      val scriptResult = InvokeScriptResult(
        leases = Seq(InvokeScriptResult.Lease(recipientAddress, 100, 1, leaseId1), InvokeScriptResult.Lease(recipientAlias, 200, 3, leaseId2)),
        leaseCancels = Seq(LeaseCancel(leaseCancelId)),
        invokes = Seq(
          InvokeScriptResult.Invocation(
            nestedInvokeAddress,
            InvokeScriptResult.Call("nested", Nil),
            Nil,
            InvokeScriptResult(
              leases = Seq(InvokeScriptResult.Lease(recipientAddress, 100, 1, nestedLeaseId)),
              leaseCancels = Seq(LeaseCancel(nestedLeaseCancelId))
            )
          )
        )
      )

      (blockchain.leaseDetails _)
        .expects(leaseId1)
        .returning(Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Active, leaseId1, 1)))
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(leaseId2)
        .returning(Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Active, leaseId2, 1)))
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(leaseCancelId)
        .returning(
          Some(
            LeaseDetails(
              TestValues.keyPair.publicKey,
              TestValues.address,
              123,
              LeaseDetails.Status.Cancelled(2, Some(leaseCancelId)),
              leaseCancelId,
              1
            )
          )
        )
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(nestedLeaseId)
        .returning(Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Active, nestedLeaseId, 1)))
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(nestedLeaseCancelId)
        .returning(
          Some(
            LeaseDetails(
              TestValues.keyPair.publicKey,
              TestValues.address,
              123,
              LeaseDetails.Status.Cancelled(2, Some(nestedLeaseCancelId)),
              nestedLeaseCancelId,
              1
            )
          )
        )
        .anyNumberOfTimes()

      (blockchain.transactionMeta _).expects(leaseId1).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseId2).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseCancelId).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(nestedLeaseId).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(nestedLeaseCancelId).returning(Some(TxMeta(Height(1), true, 0L))).anyNumberOfTimes()

      (() => blockchain.activatedFeatures).expects().returns(Map.empty).anyNumberOfTimes()
      (addressTransactions.transactionById _)
        .expects(invoke.id())
        .returning(Some(TransactionMeta.Invoke(Height(1), invoke, succeeded = true, 0L, Some(scriptResult))))
        .once()

      Get(routePath(s"/info/${invoke.id()}")) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        val json = (responseAs[JsObject] \ "stateChanges").as[JsObject]
        json should matchJson(s"""{
                                   |  "data" : [ ],
                                   |  "transfers" : [ ],
                                   |  "issues" : [ ],
                                   |  "reissues" : [ ],
                                   |  "burns" : [ ],
                                   |  "sponsorFees" : [ ],
                                   |  "leases" : [ {
                                   |    "id" : "$leaseId1",
                                   |    "originTransactionId" : "$leaseId1",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "amount" : 123,
                                   |    "height" : 1,
                                   |    "status":"active",
                                   |    "cancelHeight" : null,
                                   |    "cancelTransactionId" : null
                                   |  }, {
                                   |    "id" : "$leaseId2",
                                   |    "originTransactionId" : "$leaseId2",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "amount" : 123,
                                   |    "height" : 1,
                                   |    "status":"active",
                                   |    "cancelHeight" : null,
                                   |    "cancelTransactionId" : null
                                   |  } ],
                                   |  "leaseCancels" : [ {
                                   |    "id" : "$leaseCancelId",
                                   |    "originTransactionId" : "$leaseCancelId",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "amount" : 123,
                                   |    "height" : 1,
                                   |    "status" : "canceled",
                                   |    "cancelHeight" : 2,
                                   |    "cancelTransactionId" : "$leaseCancelId"
                                   |  } ],
                                   |  "invokes" : [ {
                                   |    "dApp" : "$nestedInvokeAddress",
                                   |    "call" : {
                                   |      "function" : "nested",
                                   |      "args" : [ ]
                                   |    },
                                   |    "payment" : [ ],
                                   |    "stateChanges" : {
                                   |      "data" : [ ],
                                   |      "transfers" : [ ],
                                   |      "issues" : [ ],
                                   |      "reissues" : [ ],
                                   |      "burns" : [ ],
                                   |      "sponsorFees" : [ ],
                                   |      "leases" : [ {
                                   |        "id" : "$nestedLeaseId",
                                   |        "originTransactionId" : "$nestedLeaseId",
                                   |        "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |        "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |        "amount" : 123,
                                   |        "height" : 1,
                                   |        "status":"active",
                                   |        "cancelHeight" : null,
                                   |        "cancelTransactionId" : null
                                   |      } ],
                                   |      "leaseCancels" : [ {
                                   |        "id" : "$nestedLeaseCancelId",
                                   |        "originTransactionId" : "$nestedLeaseCancelId",
                                   |        "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |        "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |        "amount" : 123,
                                   |        "height" : 1,
                                   |        "status" : "canceled",
                                   |        "cancelHeight" : 2,
                                   |        "cancelTransactionId" : "$nestedLeaseCancelId"
                                   |      } ],
                                   |      "invokes" : [ ]
                                   |    }
                                   |  } ]
                                   |}
                                   |""".stripMargin)
      }
    }

    "handles multiple ids" in {
      val txCount = 5
      val txs     = (1 to txCount).map(_ => TxHelpers.invoke(TxHelpers.defaultSigner.toAddress, "test"))
      txs.foreach(
        tx =>
          (addressTransactions.transactionById _)
            .expects(tx.id())
            .returns(Some(TransactionMeta.Invoke(Height(1), tx, succeeded = true, 85L, Some(InvokeScriptResult()))))
            .repeat(3)
      )

      (() => blockchain.activatedFeatures).expects().returns(Map(BF.BlockV5.id -> 1)).anyNumberOfTimes()

      def checkResponse(): Unit = txs.zip(responseAs[JsArray].value) foreach {
        case (tx, json) =>
          val extraFields =
            Json.obj("height" -> 1, "spentComplexity" -> 85, "applicationStatus" -> "succeeded", "stateChanges" -> InvokeScriptResult())
          json shouldBe (tx.json() ++ extraFields)
      }

      Get(routePath(s"/info?${txs.map("id=" + _.id()).mkString("&")}")) ~> route ~> check(checkResponse())
      Post(routePath("/info"), FormData(txs.map("id" -> _.id().toString): _*)) ~> route ~> check(checkResponse())
      Post(
        routePath("/info"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(txs.map(_.id().toString: JsValueWrapper): _*)).toString())
      ) ~> route ~> check(
        checkResponse()
      )
    }
  }

  routePath("/status/{signature}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/status?id=$invalidBase58")) ~> route should produce(InvalidIds(Seq(invalidBase58)))
      }
    }

    "handles empty request" in {
      Get(routePath(s"/status?")) ~> route should produce(CustomValidationError("Empty request"))
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx                           <- randomTransactionGen
        height                       <- Gen.chooseNum(1, 1000)
        acceptFailedActivationHeight <- Gen.chooseNum(1, 1000)
        succeed                      <- if (height >= acceptFailedActivationHeight) Arbitrary.arbBool.arbitrary else Gen.const(true)
      } yield (tx, height, acceptFailedActivationHeight, succeed)

      forAll(txAvailability) {
        case (tx, height, acceptFailedActivationHeight, succeed) =>
          (blockchain.transactionInfo _).expects(tx.id()).returning(Some(TxMeta(Height(height), succeed, 93L) -> tx)).anyNumberOfTimes()
          (() => blockchain.height).expects().returning(1000).anyNumberOfTimes()
          (() => blockchain.activatedFeatures)
            .expects()
            .returning(Map(BF.BlockV5.id -> acceptFailedActivationHeight))
            .anyNumberOfTimes()

          Get(routePath(s"/status?id=${tx.id().toString}&id=${tx.id().toString}")) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            val obj = {
              val common = Json.obj(
                "id"              -> tx.id().toString,
                "status"          -> "confirmed",
                "height"          -> JsNumber(height),
                "confirmations"   -> JsNumber(1000 - height),
                "spentComplexity" -> 93
              )
              val applicationStatus =
                if (blockchain.isFeatureActivated(BF.BlockV5, height))
                  Json.obj("applicationStatus" -> JsString(if (succeed) "succeeded" else "script_execution_failed"))
                else Json.obj()
              common ++ applicationStatus
            }
            responseAs[JsValue] shouldEqual Json.arr(obj, obj)
          }
          Post(routePath("/status"), Json.obj("ids" -> Seq(tx.id().toString, tx.id().toString))) ~> route ~> check {
            status shouldEqual StatusCodes.OK
          }
      }
    }
  }

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (() => addressTransactions.unconfirmedTransactions).expects().returning(txs).once()
        Get(routePath("/unconfirmed")) ~> route ~> check {
          val resp = responseAs[Seq[JsValue]]
          for ((r, t) <- resp.zip(txs)) {
            if ((r \ "version").as[Int] == 1) {
              (r \ "signature").as[String] shouldEqual t.proofs.proofs.head.toString
            } else {
              (r \ "proofs").as[Seq[String]] shouldEqual t.proofs.proofs.map(_.toString)
            }
          }
        }
      }
    }
  }

  routePath("/unconfirmed/size") - {
    "returns the size of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        utxPoolSize.expects().returning(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{id}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidTransactionId("Wrong char"), matchMsg = true)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomTransactionGen) { tx =>
        (addressTransactions.unconfirmedTransactionById _).expects(tx.id()).returns(Some(tx)).once()
        Get(routePath(s"/unconfirmed/info/${tx.id().toString}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json()
        }
      }
    }
  }

  routePath("/sign") - {
    "function call without args" in {
      val acc1 = testWallet.generateNewAccount().get
      val acc2 = testWallet.generateNewAccount().get

      val funcName          = "func"
      val funcWithoutArgs   = Json.obj("function" -> funcName)
      val funcWithEmptyArgs = Json.obj("function" -> funcName, "args" -> JsArray.empty)
      val funcWithArgs = InvokeScriptTransaction.serializer.functionCallToJson(
        FUNCTION_CALL(
          FunctionHeader.User(funcName),
          List(CONST_LONG(1), CONST_BOOLEAN(true))
        )
      )

      def invoke(func: JsObject, expectedArgsLength: Int): Unit = {
        val ist = Json.obj(
          "type"       -> InvokeScriptTransaction.typeId,
          "version"    -> Gen.oneOf(InvokeScriptTransaction.supportedVersions.toSeq).sample.get,
          "sender"     -> acc1.toAddress,
          "dApp"       -> acc2.toAddress,
          "call"       -> func,
          "payment"    -> Seq[Payment](),
          "fee"        -> 500000,
          "feeAssetId" -> JsNull
        )
        Post(routePath("/sign"), ist) ~> ApiKeyHeader ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val jsObject = responseAs[JsObject]
          (jsObject \ "senderPublicKey").as[String] shouldBe acc1.publicKey.toString
          (jsObject \ "call" \ "function").as[String] shouldBe funcName
          (jsObject \ "call" \ "args").as[JsArray].value.length shouldBe expectedArgsLength
        }
      }

      invoke(funcWithoutArgs, 0)
      invoke(funcWithEmptyArgs, 0)
      invoke(funcWithArgs, 2)
    }
  }

  routePath("/broadcast") - {
    def withInvokeScriptTransaction(f: (KeyPair, InvokeScriptTransaction) => Unit): Unit = {
      val seed = new Array[Byte](32)
      Random.nextBytes(seed)
      val sender: KeyPair = KeyPair(seed)
      val ist = InvokeScriptTransaction(
        TxVersion.V1,
        sender.publicKey,
        sender.toAddress,
        None,
        Seq.empty,
        500000L,
        Asset.Waves,
        testTime.getTimestamp(),
        Proofs.empty,
        AddressScheme.current.chainId
      ).signWith(sender.privateKey)
      f(sender, ist)
    }

    "shows trace when trace is enabled" in withInvokeScriptTransaction { (sender, ist) =>
      val accountTrace = AccountVerifierTrace(sender.toAddress, Some(GenericError("Error in account script")))
      (utxPoolSynchronizer.validateAndBroadcast _)
        .expects(*, None)
        .returning(
          Future.successful(TracedResult(Right(true), List(accountTrace)))
        )
        .once()
      Post(routePath("/broadcast?trace=true"), ist.json()) ~> route ~> check {
        val result = responseAs[JsObject]
        (result \ "trace").as[JsValue] shouldBe Json.arr(accountTrace.json)
      }
    }

    "does not show trace when trace is disabled" in withInvokeScriptTransaction { (sender, ist) =>
      val accountTrace = AccountVerifierTrace(sender.toAddress, Some(GenericError("Error in account script")))
      (utxPoolSynchronizer.validateAndBroadcast _)
        .expects(*, None)
        .returning(
          Future.successful(TracedResult(Right(true), List(accountTrace)))
        )
        .twice()
      Post(routePath("/broadcast"), ist.json()) ~> route ~> check {
        (responseAs[JsObject] \ "trace") shouldBe empty
      }
      Post(routePath("/broadcast?trace=false"), ist.json()) ~> route ~> check {
        (responseAs[JsObject] \ "trace") shouldBe empty
      }
    }

    "generates valid trace with vars" in withDomain(settingsWithFeatures(BF.SmartAccounts, BF.BlockV5, BF.SynchronousCalls, BF.Ride4DApps)) { d =>
      val sender     = testWallet.generateNewAccount().get
      val aliasOwner = testWallet.generateNewAccount().get
      val recipient  = testWallet.generateNewAccount().get

      val lease = LeaseTransaction.selfSigned(2.toByte, sender, recipient.toAddress, 50.waves, 0.001.waves, ntpTime.getTimestamp()).explicitGet()

      d.appendBlock(
        GenesisTransaction.create(sender.toAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(aliasOwner.toAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        CreateAliasTransaction.selfSigned(2.toByte, aliasOwner, "test_alias", 0.001.waves, ntpTime.getTimestamp()).explicitGet(),
        SetScriptTransaction
          .selfSigned(
            2.toByte,
            sender,
            Some(TestCompiler(V5).compileContract(s"""{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-# SCRIPT_TYPE ACCOUNT #-}
             |
             |@Callable(i)
             |func default() = {
             |  let leaseToAddress = Lease(Address(base58'${recipient.toAddress}'), ${10.waves})
             |  let leaseToAlias = Lease(Alias("test_alias"), ${20.waves})
             |  strict leaseId = leaseToAddress.calculateLeaseId()
             |
             |  [
             |    leaseToAddress,
             |    leaseToAlias,
             |    LeaseCancel(base58'${lease.id()}')
             |  ]
             |}
             |""".stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
          .explicitGet(),
        lease
      )

      val invoke = InvokeScriptTransaction
        .selfSigned(2.toByte, sender, sender.toAddress, None, Seq.empty, 0.005.waves, Asset.Waves, ntpTime.getTimestamp())
        .explicitGet()

      Post(routePath("/broadcast?trace=true"), invoke.json()) ~> mkRoute(d) ~> check {
        val dappTrace = (responseAs[JsObject] \ "trace").as[Seq[JsObject]].find(jsObject => (jsObject \ "type").as[String] == "dApp").get

        (dappTrace \ "error").get shouldEqual JsNull
        (dappTrace \ "vars" \\ "name").map(_.as[String]) should contain theSameElementsAs Seq("leaseToAddress", "leaseToAlias", "leaseId")
      }
    }
  }

  routePath("/merkleProof") - {
    val transactionsGen = for {
      txsSize <- Gen.choose(1, 10)
      txs     <- Gen.listOfN(txsSize, randomTransactionGen)
    } yield txs

    val invalidBlockGen = for {
      txs     <- transactionsGen
      signer  <- accountGen
      version <- Gen.choose(Block.GenesisBlockVersion, Block.RewardBlockVersion)
      block   <- versionedBlockGen(txs, signer, version)
    } yield block

    val invalidBlocksGen =
      for {
        blockchainHeight <- Gen.choose(1, 10)
        blocks           <- Gen.listOfN(blockchainHeight, invalidBlockGen)
      } yield blocks

    val merkleProofs = for {
      index        <- Gen.choose(0, 50)
      tx           <- randomTransactionGen
      proofsLength <- Gen.choose(1, 5)
      proofBytes   <- Gen.listOfN(proofsLength, bytes32gen)
    } yield (tx, TransactionProof(tx.id(), index, proofBytes))

    def validateSuccess(expectedProofs: Seq[TransactionProof], response: HttpResponse): Unit = {
      response.status shouldBe StatusCodes.OK

      val proofs = responseAs[List[JsObject]]

      proofs.size shouldBe expectedProofs.size

      proofs.zip(expectedProofs).foreach {
        case (p, e) =>
          val transactionId    = (p \ "id").as[String]
          val transactionIndex = (p \ "transactionIndex").as[Int]
          val digests          = (p \ "merkleProof").as[List[String]].map(s => ByteStr.decodeBase58(s).get)

          transactionId shouldEqual e.id.toString
          transactionIndex shouldEqual e.transactionIndex
          digests shouldEqual e.digests.map(ByteStr(_))
      }
    }

    def validateFailure(response: HttpResponse): Unit = {
      response.status shouldEqual StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldEqual s"transactions do not exist or block version < ${Block.ProtoBlockVersion}"
    }

    "returns merkle proofs" in {
      forAll(Gen.choose(10, 20).flatMap(n => Gen.listOfN(n, merkleProofs))) { transactionsAndProofs =>
        val (transactions, proofs) = transactionsAndProofs.unzip
        (addressTransactions.transactionProofs _).expects(transactions.map(_.id())).returning(proofs).twice()

        val queryParams = transactions.map(t => s"id=${t.id()}").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> transactions.map(_.id().toString))

        Get(routePath(s"/merkleProof$queryParams")) ~> route ~> check {
          validateSuccess(proofs, response)
        }

        Post(routePath("/merkleProof"), requestBody) ~> route ~> check {
          validateSuccess(proofs, response)
        }
      }
    }

    "returns error in case of all transactions are filtered" in {
      forAll(invalidBlocksGen) { blocks =>
        val txIdsToBlock = blocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap

        val queryParams = txIdsToBlock.keySet.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> txIdsToBlock.keySet)

        (addressTransactions.transactionProofs _).expects(*).returning(Nil).anyNumberOfTimes()

        Get(routePath(s"/merkleProof$queryParams")) ~> route ~> check {
          validateFailure(response)
        }

        Post(routePath("/merkleProof"), requestBody) ~> route ~> check {
          validateFailure(response)
        }
      }
    }

    "handles invalid signatures" in {
      val invalidIdsGen = for {
        ids       <- Gen.nonEmptyListOf(randomTransactionGen.map(_.id().toString))
        invalidId <- Gen.nonEmptyListOf(invalidBase58Gen)
      } yield Random.shuffle(ids ++ invalidId)

      forAll(invalidIdsGen) { invalidIds =>
        val queryParams = invalidIds.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> invalidIds)

        Get(routePath(s"/merkleProof$queryParams")) ~> route should produce(InvalidSignature)

        Post(routePath("/merkleProof"), requestBody) ~> route should produce(InvalidSignature)
      }
    }
  }
}
