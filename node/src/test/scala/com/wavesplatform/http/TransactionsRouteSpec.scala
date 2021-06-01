package com.wavesplatform.http

import akka.http.scaladsl.model._
import com.wavesplatform.account.{AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, _}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, FUNCTION_CALL, TRUE}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.traits.domain.{Lease, LeaseCancel, Recipient}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Height, InvokeScriptResult}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, TracedResult}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, Proofs, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.{BlockGen, BlockchainStubHelpers, NoShrink, TestTime, TestValues, TestWallet, TransactionGen}
import monix.reactive.Observable
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Random

class TransactionsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with Matchers
    with TransactionGen
    with BlockGen
    with PropertyChecks
    with OptionValues
    with TestWallet
    with NoShrink
    with BlockchainStubHelpers {

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
    "transfer with Waves fee" - {
      val transferTxScenario =
        for {
          sender    <- accountGen
          recipient <- accountGen
          version   <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
          tx = Json.obj(
            "type"            -> 4,
            "version"         -> version,
            "amount"          -> 1000000,
            "senderPublicKey" -> sender.publicKey.toString,
            "recipient"       -> recipient.toAddress
          )
        } yield (sender.publicKey, tx)
      "TransferTransaction" in forAll(transferTxScenario) {
        case (_, transferTx) =>
          (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 100000L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
          }
      }

      val massTransferTxScenario =
        for {
          sender     <- accountGen
          recipient1 <- accountGen
          recipient2 <- accountGen
          version    <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
          tx = Json.obj(
            "type"            -> 11,
            "version"         -> version,
            "senderPublicKey" -> Base58.encode(sender.publicKey.arr),
            "transfers" -> Json.arr(
              Json.obj(
                "recipient" -> recipient1.toAddress,
                "amount"    -> 1000000
              ),
              Json.obj(
                "recipient" -> recipient2.toAddress,
                "amount"    -> 2000000
              )
            )
          )
        } yield (sender.publicKey, tx)
      "MassTransferTransaction" in forAll(massTransferTxScenario) {
        case (_, transferTx) =>
          (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 200000L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 200000
          }
      }
    }

    "transfer with Asset fee" - {
      val transferTxWithAssetFeeScenario =
        for {
          assetId   <- issueGen.map(_.assetId)
          sender    <- accountGen
          recipient <- accountGen
          version   <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
          tx = Json.obj(
            "type"            -> 4,
            "version"         -> version,
            "amount"          -> 1000000,
            "feeAssetId"      -> assetId.toString,
            "senderPublicKey" -> Base58.encode(sender.publicKey.arr),
            "recipient"       -> recipient.toAddress
          )
        } yield (sender.publicKey, tx, IssuedAsset(assetId))
      "without sponsorship" in forAll(transferTxWithAssetFeeScenario) {
        case (_, transferTx, _) =>
          (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 100000L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
          }
      }

      "with sponsorship" in {
        val assetId: IssuedAsset = IssuedAsset(issueGen.sample.get.assetId)
        val sender: PublicKey    = accountGen.sample.get.publicKey
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.id.toString,
          "senderPublicKey" -> Base58.encode(sender.arr),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        (addressTransactions.calculateFee _).expects(*).returning(Right((assetId, 5L, 0L))).once()

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe assetId.id.toString
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 5
        }
      }

      "with sponsorship, smart token and smart account" in {
        val assetId: IssuedAsset = IssuedAsset(issueGen.sample.get.assetId)
        val sender: PublicKey    = accountGen.sample.get.publicKey
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.id.toString,
          "senderPublicKey" -> Base58.encode(sender.arr),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        (addressTransactions.calculateFee _).expects(*).returning(Right((assetId, 45L, 0L))).once()

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe assetId.id.toString
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 45
        }
      }

      s"after ${BlockchainFeatures.SynchronousCalls}" in {
        val blockchain = createBlockchainStub { blockchain =>
          val settings = TestFunctionalitySettings.Enabled.copy(
            preActivatedFeatures = Map(
              BlockchainFeatures.SmartAccounts.id    -> 0,
              BlockchainFeatures.SmartAssets.id      -> 0,
              BlockchainFeatures.Ride4DApps.id       -> 0,
              BlockchainFeatures.FeeSponsorship.id   -> 0,
              BlockchainFeatures.DataTransaction.id  -> 0,
              BlockchainFeatures.BlockReward.id      -> 0,
              BlockchainFeatures.BlockV5.id          -> 0,
              BlockchainFeatures.SynchronousCalls.id -> 0
            ),
            featureCheckBlocksPeriod = 1,
            blocksForFeatureActivation = 1
          )
          (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings.copy(functionalitySettings = settings))
          (() => blockchain.activatedFeatures).when().returns(settings.preActivatedFeatures)
          (blockchain.balance _).when(*, *).returns(ENOUGH_AMT)

          val script                = ExprScript(TRUE).explicitGet()
          def info(complexity: Int) = Some(AccountScriptInfo(TxHelpers.secondSigner.publicKey, script, complexity))

          (blockchain.accountScript _).when(TxHelpers.defaultSigner.toAddress).returns(info(199))
          (blockchain.accountScript _).when(TxHelpers.secondSigner.toAddress).returns(info(201))
          (blockchain.accountScript _).when(TxHelpers.signer(3).toAddress).returns(None)
        }
        val route = seal(transactionsApiRoute.copy(blockchain = blockchain).route)

        (addressTransactions.calculateFee _)
          .expects(*)
          .onCall(
            (tx: Transaction) =>
              FeeValidation
                .getMinFee(blockchain, tx)
                .map {
                  case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
                    (asset, feeInAsset, feeInWaves)
                }
          )
          .anyNumberOfTimes()

        val tx1 = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1,
          "senderPublicKey" -> Base58.encode(TxHelpers.defaultSigner.publicKey.arr),
          "recipient"       -> accountGen.sample.get.toAddress
        )
        Post(routePath("/calculateFee"), tx1) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
        }

        val tx2 = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1,
          "senderPublicKey" -> Base58.encode(TxHelpers.secondSigner.publicKey.arr),
          "recipient"       -> accountGen.sample.get.toAddress
        )
        Post(routePath("/calculateFee"), tx2) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 500000
        }

        val tx3 = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1,
          "senderPublicKey" -> Base58.encode(TxHelpers.signer(3).publicKey.arr),
          "recipient"       -> accountGen.sample.get.toAddress
        )
        Post(routePath("/calculateFee"), tx3) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
        }
      }
    }
  }

  routePath("/address/{address}/limit/{limit}") - {
    val bytes32StrGen = bytes32gen.map(Base58.encode)
    val addressGen    = accountGen.map(_.toAddress.toString)

    "returns lease tx for lease cancel tx" in {
      val lease       = TxHelpers.lease()
      val leaseCancel = TxHelpers.leaseCancel(lease.id())

      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.transactionInfo _).when(lease.id()).returns(Some((1, lease, true)))
        (blockchain.transactionInfo _).when(leaseCancel.id()).returns(Some((1, leaseCancel, true)))
      }

      val transactionsApi = stub[CommonTransactionsApi]
      (transactionsApi.transactionById _).when(lease.id()).returns(Some(TransactionMeta.Default(Height(1), lease, succeeded = true)))
      (transactionsApi.transactionById _).when(leaseCancel.id()).returns(Some(TransactionMeta.Default(Height(1), leaseCancel, succeeded = true)))
      (transactionsApi.transactionsByAddress _)
        .when(TxHelpers.secondAddress, None, *, None)
        .returns(Observable(TransactionMeta.Default(Height(1), leaseCancel, succeeded = true)))
      (transactionsApi.aliasesOfAddress _).when(*).returns(Observable.empty)
      (blockchain.transactionMeta _).when(lease.id()).returns(Some((1, true)))
      (blockchain.leaseDetails _)
        .when(lease.id())
        .returns(Some(LeaseDetails(lease.sender, lease.recipient, lease.amount, LeaseDetails.Status.Cancelled(2, Some(leaseCancel.id())), lease.id(), 1)))

      val route = transactionsApiRoute.copy(blockchain = blockchain, commonApi = transactionsApi).route
      Get(routePath(s"/address/${TxHelpers.secondAddress}/limit/10")) ~> route ~> check {
        val json = (responseAs[JsArray] \ 0 \ 0).as[JsObject]
        json shouldBe Json.parse(s"""{
                                   |  "type" : 9,
                                   |  "id" : "${leaseCancel.id()}",
                                   |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |  "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
                                   |  "fee" : 1000000,
                                   |  "feeAssetId" : null,
                                   |  "timestamp" : ${leaseCancel.timestamp},
                                   |  "proofs" : [ "${leaseCancel.signature}" ],
                                   |  "version" : 2,
                                   |  "leaseId" : "${lease.id()}",
                                   |  "chainId" : 84,
                                   |  "height" : 1,
                                   |  "applicationStatus" : "succeeded",
                                   |  "lease" : {
                                   |    "id" : "${lease.id()}",
                                   |    "originTransactionId" : "${lease.id()}",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                   |    "amount" : 1000000000,
                                   |    "height" : 1,
                                   |    "status" : "canceled"
                                   |  }
                                   |}""".stripMargin)
      }
    }

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
        .returning(Observable(TransactionMeta.Invoke(Height(1), transaction, succeeded = true, Some(InvokeScriptResult()))))
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
          Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Cancelled(2, Some(leaseCancelId)), leaseCancelId, 1))
        )
        .anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseId1).returning(Some((1, true))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseId2).returning(Some((1, true))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseCancelId).returning(Some((1, true))).anyNumberOfTimes()

      (() => blockchain.activatedFeatures).expects().returning(Map.empty).anyNumberOfTimes()
      (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once()
      (addressTransactions.transactionsByAddress _)
        .expects(invokeAddress, *, *, None)
        .returning(Observable(TransactionMeta.Invoke(Height(1), invoke, succeeded = true, Some(scriptResult))))
        .once()

      Get(routePath(s"/address/${invokeAddress}/limit/1")) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        val json = (responseAs[JsArray] \ 0 \ 0 \ "stateChanges").as[JsObject]
        json shouldBe Json.parse(s"""{
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
                                    |      "status":"active"
                                    |    },
                                    |    {
                                    |      "id": "$leaseId2",
                                    |      "originTransactionId": "$leaseId2",
                                    |      "sender": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "recipient": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                    |      "amount": 123,
                                    |      "height": 1,
                                    |      "status":"active"
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
                                    |      "status":"canceled"
                                    |    }
                                    |  ],
                                    |  "invokes": []
                                    |}""".stripMargin)
      }
    }
  }

  routePath("/info/{id}") - {
    "returns lease tx for lease cancel tx" in {
      val lease       = TxHelpers.lease()
      val leaseCancel = TxHelpers.leaseCancel(lease.id())

      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.transactionInfo _).when(lease.id()).returns(Some((1, lease, true)))
        (blockchain.transactionInfo _).when(leaseCancel.id()).returns(Some((1, leaseCancel, true)))
      }

      val transactionsApi = stub[CommonTransactionsApi]
      (transactionsApi.transactionById _).when(lease.id()).returns(Some(TransactionMeta.Default(Height(1), lease, succeeded = true)))
      (transactionsApi.transactionById _).when(leaseCancel.id()).returns(Some(TransactionMeta.Default(Height(1), leaseCancel, succeeded = true)))
      (blockchain.transactionMeta _).when(lease.id()).returns(Some((1, true)))
      (blockchain.leaseDetails _)
        .when(lease.id())
        .returns(Some(LeaseDetails(lease.sender, lease.recipient, lease.amount, LeaseDetails.Status.Cancelled(2, Some(leaseCancel.id())), lease.id(), 1)))

      val route = transactionsApiRoute.copy(blockchain = blockchain, commonApi = transactionsApi).route
      Get(routePath(s"/info/${leaseCancel.id()}")) ~> route ~> check {
        val json = responseAs[JsObject]
        json shouldBe Json.parse(s"""{
                                   |  "type" : 9,
                                   |  "id" : "${leaseCancel.id()}",
                                   |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |  "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
                                   |  "fee" : 1000000,
                                   |  "feeAssetId" : null,
                                   |  "timestamp" : ${leaseCancel.timestamp},
                                   |  "proofs" : [ "${leaseCancel.signature}" ],
                                   |  "version" : 2,
                                   |  "leaseId" : "${lease.id()}",
                                   |  "chainId" : 84,
                                   |  "height" : 1,
                                   |  "applicationStatus" : "succeeded",
                                   |  "lease" : {
                                   |    "id" : "${lease.id()}",
                                   |    "originTransactionId" : "${lease.id()}",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                   |    "amount" : 1000000000,
                                   |    "height" : 1,
                                   |    "status" : "canceled"
                                   |  }
                                   |}""".stripMargin)
      }
    }

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
          (addressTransactions.transactionById _).expects(tx.id()).returning(Some(TransactionMeta.Default(Height(height), tx, succeed))).once()
          (() => blockchain.activatedFeatures)
            .expects()
            .returning(Map(BlockchainFeatures.BlockV5.id -> acceptFailedActivationHeight))
            .anyNumberOfTimes()

          def validateResponse(): Unit = {
            status shouldEqual StatusCodes.OK

            val extraFields =
              if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height))
                Json.obj("height"    -> height, "applicationStatus" -> JsString(if (succeed) "succeeded" else "script_execution_failed"))
              else Json.obj("height" -> height)
            responseAs[JsValue] shouldEqual (tx.json() ++ extraFields)
          }

          Get(routePath(s"/info/${tx.id().toString}")) ~> route ~> check(validateResponse())
      }
    }

    "provides stateChanges" in forAll(accountGen) { account =>
      val transaction = TxHelpers.invoke(account.toAddress, "test")

      (() => blockchain.activatedFeatures).expects().returns(Map.empty).anyNumberOfTimes()
      (addressTransactions.transactionById _)
        .expects(transaction.id())
        .returning(Some(TransactionMeta.Invoke(Height(1), transaction, succeeded = true, Some(InvokeScriptResult()))))
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
          Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Cancelled(2, Some(leaseCancelId)), leaseCancelId, 1))
        )
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(nestedLeaseId)
        .returning(Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Active, nestedLeaseId, 1)))
        .anyNumberOfTimes()
      (blockchain.leaseDetails _)
        .expects(nestedLeaseCancelId)
        .returning(
          Some(LeaseDetails(TestValues.keyPair.publicKey, TestValues.address, 123, LeaseDetails.Status.Cancelled(2, Some(nestedLeaseCancelId)), nestedLeaseCancelId, 1))
        )
        .anyNumberOfTimes()

      (blockchain.transactionMeta _).expects(leaseId1).returning(Some((1, true))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseId2).returning(Some((1, true))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(leaseCancelId).returning(Some((1, true))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(nestedLeaseId).returning(Some((1, true))).anyNumberOfTimes()
      (blockchain.transactionMeta _).expects(nestedLeaseCancelId).returning(Some((1, true))).anyNumberOfTimes()

      (() => blockchain.activatedFeatures).expects().returns(Map.empty).anyNumberOfTimes()
      (addressTransactions.transactionById _)
        .expects(invoke.id())
        .returning(Some(TransactionMeta.Invoke(Height(1), invoke, succeeded = true, Some(scriptResult))))
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
                                   |    "status" : "active"
                                   |  }, {
                                   |    "id" : "$leaseId2",
                                   |    "originTransactionId" : "$leaseId2",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "amount" : 123,
                                   |    "height" : 1,
                                   |    "status" : "active"
                                   |  } ],
                                   |  "leaseCancels" : [ {
                                   |    "id" : "$leaseCancelId",
                                   |    "originTransactionId" : "$leaseCancelId",
                                   |    "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |    "amount" : 123,
                                   |    "height" : 1,
                                   |    "status" : "canceled"
                                   |  } ],
                                   |  "invokes" : [ {
                                   |    "dApp" : "$nestedInvokeAddress",
                                   |    "call" : {
                                   |      "function" : "nested",
                                   |      "args" : [ ]
                                   |    },
                                   |    "payments" : [ ],
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
                                   |        "status" : "active"
                                   |      } ],
                                   |      "leaseCancels" : [ {
                                   |        "id" : "$nestedLeaseCancelId",
                                   |        "originTransactionId" : "$nestedLeaseCancelId",
                                   |        "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |        "recipient" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |        "amount" : 123,
                                   |        "height" : 1,
                                   |        "status" : "canceled"
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
            .returns(Some(TransactionMeta.Invoke(Height(1), tx, succeeded = true, Some(InvokeScriptResult()))))
            .repeat(3)
      )

      (() => blockchain.activatedFeatures).expects().returns(Map(BlockchainFeatures.BlockV5.id -> 1)).anyNumberOfTimes()

      def checkResponse(): Unit = txs.zip(responseAs[JsArray].value) foreach {
        case (tx, json) =>
          val extraFields = Json.obj("height" -> 1, "applicationStatus" -> "succeeded", "stateChanges" -> InvokeScriptResult())
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
          (blockchain.transactionInfo _).expects(tx.id()).returning(Some((height, tx, succeed))).anyNumberOfTimes()
          (() => blockchain.height).expects().returning(1000).anyNumberOfTimes()
          (() => blockchain.activatedFeatures)
            .expects()
            .returning(Map(BlockchainFeatures.BlockV5.id -> acceptFailedActivationHeight))
            .anyNumberOfTimes()

          Get(routePath(s"/status?id=${tx.id().toString}&id=${tx.id().toString}")) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            val obj = {
              val common = Json.obj(
                "id"            -> tx.id().toString,
                "status"        -> "confirmed",
                "height"        -> JsNumber(height),
                "confirmations" -> JsNumber(1000 - height)
              )
              val applicationStatus =
                if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height))
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
    "returns lease tx for lease cancel tx" in {
      val lease       = TxHelpers.lease()
      val leaseCancel = TxHelpers.leaseCancel(lease.id())

      val blockchain = createBlockchainStub { blockchain =>
        (blockchain.transactionInfo _).when(lease.id()).returns(Some((1, lease, true)))
        (blockchain.transactionInfo _).when(leaseCancel.id()).returns(Some((1, leaseCancel, true)))
        (blockchain.transactionMeta _).when(lease.id()).returns(Some((1, true)))
        (blockchain.leaseDetails _).when(lease.id()).returns(Some(LeaseDetails(lease.sender, lease.recipient, lease.amount, LeaseDetails.Status.Active, lease.id(), 1)))
      }

      val transactionsApi = stub[CommonTransactionsApi]
      (transactionsApi.transactionById _).when(lease.id()).returns(Some(TransactionMeta.Default(Height(1), lease, succeeded = true)))
      (transactionsApi.transactionById _).when(leaseCancel.id()).returns(Some(TransactionMeta.Default(Height(1), leaseCancel, succeeded = true)))
      (() => transactionsApi.unconfirmedTransactions).when().returns(Seq(leaseCancel))
      (transactionsApi.unconfirmedTransactionById _).when(leaseCancel.id()).returns(Some(leaseCancel))
      (transactionsApi.aliasesOfAddress _).when(*).returns(Observable.empty)

      val route = transactionsApiRoute.copy(blockchain = blockchain, commonApi = transactionsApi).route

      val leaseDetailsJson = Json.obj(
        "id" -> lease.id(),
        "originTransactionId" -> lease.id(),
        "sender" -> lease.sender.toAddress,
        "recipient" -> lease.recipient,
        "amount" -> lease.amount,
        "height" -> 1,
        "status" -> LeaseDetails.Status.Active.toString.toLowerCase
      )

      Get(routePath(s"/unconfirmed")) ~> route ~> check {
        val json = (responseAs[JsArray] \ 0).as[JsObject]
        json shouldBe leaseCancel.json() ++ Json.obj("lease" -> leaseDetailsJson)
      }

      Get(routePath(s"/unconfirmed/info/${leaseCancel.id()}")) ~> route ~> check {
        val json = responseAs[JsObject]
        json shouldBe leaseCancel.json() ++ Json.obj("lease" -> leaseDetailsJson)
      }
    }

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

    "generates valid trace with vars" in {
      val invoke        = TxHelpers.invoke(TxHelpers.defaultAddress, "test")
      val leaseCancelId = ByteStr(bytes32gen.sample.get)

      val amount1    = 100
      val nonce1     = 0
      val recipient1 = Recipient.Address(ByteStr.decodeBase58("3NAgxLPGnw3RGv9JT6NTDaG5D1iLUehg2xd").get)
      val leaseId1   = Lease.calculateId(Lease(recipient1, amount1, nonce1), invoke.id.value())

      val amount2    = 20
      val nonce2     = 2
      val recipient2 = Recipient.Alias("some_alias")
      val leaseId2   = Lease.calculateId(Lease(recipient2, amount2, nonce2), invoke.id.value())

      val blockchain = createBlockchainStub { blockchain =>
        val (dAppScript, _) = ScriptCompiler
          .compile(
            s"""
               |{-# STDLIB_VERSION 5 #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |{-# CONTENT_TYPE DAPP #-}
               |
               |@Callable(i)
               |func test() = {
               |  let test = 1
               |  if (test == 1)
               |    then
               |      [
               |        Lease(Address(base58'${recipient1.bytes}'), $amount1, $nonce1),
               |        Lease(Alias("${recipient2.name}"), $amount2, $nonce2),
               |        LeaseCancel(base58'$leaseCancelId')
               |      ]
               |    else []
               |}
               |""".stripMargin,
            ScriptEstimatorV3
          )
          .explicitGet()

        (blockchain.leaseDetails _)
          .when(*)
          .returns(None)
          .anyNumberOfTimes()
        (blockchain.resolveAlias _)
          .when(*)
          .returns(Right(accountGen.sample.get.toAddress))
          .anyNumberOfTimes()
        (blockchain.accountScript _)
          .when(*)
          .returns(
            Some(
              AccountScriptInfo(
                TxHelpers.defaultSigner.publicKey,
                dAppScript,
                0L,
                Map(3 -> Seq("test").map(_ -> 0L).toMap)
              )
            )
          )

        (blockchain.hasAccountScript _).when(*).returns(true)
      }
      val publisher = createTxPublisherStub(blockchain)
      val route     = transactionsApiRoute.copy(blockchain = blockchain, transactionPublisher = publisher).route

      Post(routePath("/broadcast?trace=true"), invoke.json()) ~> route ~> check {
        responseAs[JsObject] shouldBe Json.parse(
          s"""{
            |  "type" : 16,
            |  "id" : "${invoke.id()}",
            |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
            |  "senderPublicKey" : "9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ",
            |  "fee" : 1000000,
            |  "feeAssetId" : null,
            |  "timestamp" : ${invoke.timestamp},
            |  "proofs" : [ "${invoke.signature}" ],
            |  "version" : 1,
            |  "dApp" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
            |  "payment" : [ ],
            |  "call" : {
            |    "function" : "test",
            |    "args" : [ ]
            |  },
            |  "trace" : [ {
            |    "type" : "verifier",
            |    "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
            |    "result" : "success",
            |    "error" : null
            |  }, {
            |    "type" : "dApp",
            |    "id" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
            |    "function" : "test",
            |    "args" : [ ],
            |    "result" : {
            |      "data" : [ ],
            |      "transfers" : [ ],
            |      "issues" : [ ],
            |      "reissues" : [ ],
            |      "burns" : [ ],
            |      "sponsorFees" : [ ],
            |      "leases" : [
            |         {
            |           "recipient" : "${recipient1.bytes}",
            |           "amount" : $amount1,
            |           "nonce" : $nonce1,
            |           "id" : "$leaseId1"
            |         },
            |         {
            |           "recipient" : "alias:T:${recipient2.name}",
            |           "amount" : $amount2,
            |           "nonce" : $nonce2,
            |           "id" : "$leaseId2"
            |         }
            |      ],
            |      "leaseCancels" : [
            |         {
            |            "id":"$leaseCancelId"
            |         }
            |      ],
            |      "invokes" : [ ]
            |    },
            |    "error" : null,
            |    "vars" : [ {
            |      "name" : "test",
            |      "type" : "Int",
            |      "value" : 1
            |    } ]
            |  } ]
            |}""".stripMargin
        )
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
