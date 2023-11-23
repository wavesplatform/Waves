package com.wavesplatform.http

import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.headers.Accept
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.{ScriptExecutionError as _, *}
import com.wavesplatform.api.http.{CustomJson, RouteTimeout, TransactionsApiRoute}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, *}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures as BF
import com.wavesplatform.history.defaultSigner
import com.wavesplatform.lang.directives.values.{V5, V7}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, EmptyDataEntry, InvokeScriptResult, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.defaultAddress
import com.wavesplatform.transaction.TxValidationError.ScriptExecutionError
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.AccountVerifierTrace
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Asset, AssetIdLength, EthTxGenerator, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.{EthEncoding, EthHelpers, SharedSchedulerMixin}
import com.wavesplatform.{BlockGen, TestValues}
import org.scalacheck.Gen
import org.scalacheck.Gen.*
import org.scalatest.{Assertion, OptionValues}
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Random

class TransactionsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with BlockGen
    with OptionValues
    with SharedDomain
    with EthHelpers
    with SharedSchedulerMixin {

  private val testTime = new TestTime

  private val richAccount = TxHelpers.signer(10001)
  private val richAddress = richAccount.toAddress

  override def settings: WavesSettings               = DomainPresets.TransactionStateSnapshot.copy(restAPISettings = restAPISettings)
  override def genesisBalances: Seq[AddrWithBalance] = Seq(AddrWithBalance(richAddress, 1_000_000.waves))



  private val transactionsApiRoute = new TransactionsApiRoute(
    settings.restAPISettings,
    domain.transactionsApi,
    domain.wallet,
    domain.blockchain,
    () => domain.blockchain,
    () => domain.utxPool.size,
    (tx, _) => Future.successful(domain.utxPool.putIfNew(tx, forceValidate = true)),
    testTime,
    new RouteTimeout(60.seconds)(sharedScheduler)
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

      Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
        (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
      }
    }

    "asset" in {
      val issuer = TxHelpers.signer(270)
      val issue  = TxHelpers.issue(issuer)

      domain.appendBlock(
        TxHelpers.transfer(richAccount, issuer.toAddress, 20.waves),
        issue,
        TxHelpers.sponsor(issue.asset, Some(5L), issuer)
      )

      val transferTx = Json.obj(
        "type"            -> 4,
        "version"         -> 2,
        "amount"          -> 1000000,
        "feeAssetId"      -> issue.asset.id.toString,
        "senderPublicKey" -> TestValues.keyPair.publicKey,
        "recipient"       -> TestValues.address
      )

      Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe issue.asset.id.toString
        (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 5
      }
    }
  }

  "returns lease details for lease cancel transaction" in {
    val sender    = TxHelpers.signer(20)
    val recipient = TxHelpers.signer(21)

    val lease       = TxHelpers.lease(sender, recipient.toAddress, 5.waves)
    val leaseCancel = TxHelpers.leaseCancel(lease.id(), sender)

    domain.appendBlock(
      TxHelpers.transfer(richAccount, sender.toAddress, 6.waves),
      lease
    )

    val leaseHeight = domain.blockchain.height

    def expectedJson(status: String, height: Int, cancelHeight: Option[Int] = None, cancelTransactionId: Option[ByteStr] = None): JsObject =
      Json
        .parse(s"""{
                  |  "applicationStatus": "succeeded",
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
                  |    "height" : $height,
                  |    "status" : "$status",
                  |    "cancelHeight" : ${cancelHeight.getOrElse("null")},
                  |    "cancelTransactionId" : ${cancelTransactionId.fold("null")("\"" + _ + "\"")}
                  |  }
                  |}""".stripMargin)
        .as[JsObject]

    domain.utxPool.putIfNew(leaseCancel)

    withClue(routePath("/unconfirmed")) {
      Get(routePath(s"/unconfirmed")) ~> route ~> check {
        responseAs[Seq[JsObject]].head should matchJson(expectedJson("active", leaseHeight) - "spentComplexity" - "applicationStatus")
      }
    }

    domain.appendBlock(leaseCancel)

    val cancelHeight = domain.blockchain.height
    val cancelTransactionJson =
      expectedJson("canceled", leaseHeight, Some(cancelHeight), Some(leaseCancel.id())) ++ Json.obj("height" -> cancelHeight)

    withClue(routePath("/address/{address}/limit/{limit}")) {
      Get(routePath(s"/address/${recipient.toAddress}/limit/10")) ~> route ~> check {
        val json = (responseAs[JsArray] \ 0 \ 0).as[JsObject]
        json should matchJson(cancelTransactionJson)
      }
    }

    withClue(routePath("/info/{id}")) {
      Get(routePath(s"/info/${leaseCancel.id()}")) ~> route ~> check {
        responseAs[JsObject] should matchJson(cancelTransactionJson)
      }
    }
  }

  "provides state changes in both transactions by address and by id" in {
    val dapp    = TxHelpers.signer(230)
    val invoker = TxHelpers.signer(231)

    val invoke = TxHelpers.invoke(dapp.toAddress)

    domain.appendBlock(
      TxHelpers.massTransfer(
        richAccount,
        Seq(
          dapp.toAddress    -> 1.waves,
          invoker.toAddress -> 1.waves
        ),
        fee = 0.002.waves
      ),
      TxHelpers.setScript(
        dapp,
        TestCompiler(V7)
          .compileContract(s"""@Callable(i)
                              |func default() = [
                              |  StringEntry("key3", "some string"),
                              |  BinaryEntry("key4", base58'encoded'),
                              |  DeleteEntry("key5"),
                              |  ScriptTransfer(Address(base58'${invoker.toAddress}'), 100, unit)
                              |]
                              |""".stripMargin)
      ),
      invoke
    )

    val expectedStateChanges = Json.toJsObject(
      InvokeScriptResult(
        Seq(StringDataEntry("key3", "some string"), BinaryDataEntry("key4", ByteStr.decodeBase58("encoded").get), EmptyDataEntry("key5")),
        Seq(InvokeScriptResult.Payment(invoker.toAddress, Asset.Waves, 100))
      )
    )

    Get(routePath(s"/address/${invoker.toAddress}/limit/1")) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsArray] \ 0 \ 0 \ "stateChanges").as[JsObject] shouldBe expectedStateChanges
    }

    Get(routePath(s"/info/${invoke.id()}")) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsObject] \ "stateChanges").as[JsObject] shouldBe expectedStateChanges
    }

    Get(routePath(s"/info?id=${invoke.id()}")) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsArray] \ 0 \ "stateChanges").as[JsObject] shouldBe expectedStateChanges
    }

    Post(
      "/transactions/info",
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(invoke.id())).toString().getBytes)
    ) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsArray] \ 0 \ "stateChanges").as[JsObject] shouldBe expectedStateChanges
    }
  }

  "provides lease and lease cancel actions stateChanges" in {
    val dapp           = TxHelpers.signer(235)
    val caller         = TxHelpers.signer(236)
    val leaseRecipient = TxHelpers.address(237)

    val originalLease = TxHelpers.lease(dapp, leaseRecipient, 20_00000000L)
    val invoke =
      TxHelpers.invoke(dapp.toAddress, Some("testLease"), Seq(CONST_LONG(1000), CONST_BYTESTR(originalLease.id()).explicitGet()), invoker = caller)

    domain.appendBlock(
      TxHelpers.massTransfer(
        richAccount,
        Seq(
          dapp.toAddress   -> 50.waves,
          caller.toAddress -> 1.waves
        ),
        fee = 0.002.waves
      ),
      originalLease,
      TxHelpers.setScript(
        dapp,
        TestCompiler(V7)
          .compileContract(s"""@Callable(i)
                              |func testLease(amount: Int, id: ByteVector) = {
                              |let lease = Lease(Address(base58'$leaseRecipient'), amount)
                              |let leaseId = calculateLeaseId(lease)
                              |[
                              |  LeaseCancel(id),
                              |  lease,
                              |  BinaryEntry("leaseId", leaseId)
                              |]
                              |}
                              |""".stripMargin)
      )
    )

    val originalHeight = domain.blockchain.height

    domain.appendBlock(invoke)

    val newLeaseId = domain.blockchain
      .accountData(dapp.toAddress, "leaseId")
      .collect { case BinaryDataEntry(_, id) => id }
      .value

    val expectedJson =
      s"""{
         |  "data": [ {
         |    "type": "binary",
         |    "key": "leaseId",
         |    "value": "${newLeaseId.base64}"
         |  } ],
         |  "transfers": [],
         |  "issues": [],
         |  "reissues": [],
         |  "burns": [],
         |  "sponsorFees": [],
         |  "leases" : [ {
         |    "id" : "$newLeaseId",
         |    "originTransactionId" : "${invoke.id()}",
         |    "sender" : "${dapp.toAddress}",
         |    "recipient" : "$leaseRecipient",
         |    "amount" : 1000,
         |    "height" : ${originalHeight + 1},
         |    "status" : "active",
         |    "cancelHeight" : null,
         |    "cancelTransactionId" : null
         |  } ],
         |  "leaseCancels" : [ {
         |    "id" : "${originalLease.id()}",
         |    "originTransactionId" : "${originalLease.id()}",
         |    "sender" : "${dapp.toAddress}",
         |    "recipient" : "$leaseRecipient",
         |    "amount" : ${20.waves},
         |    "height" : $originalHeight,
         |    "status" : "canceled",
         |    "cancelHeight" : ${originalHeight + 1},
         |    "cancelTransactionId" : "${invoke.id()}"
         |  } ],
         |  "invokes": []
         |}""".stripMargin


    Get(routePath(s"/address/${dapp.toAddress}/limit/1")) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsArray] \ 0 \ 0 \ "stateChanges").as[JsObject] should matchJson(expectedJson)
    }

    Get(routePath(s"/info/${invoke.id()}")) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsObject] \ "stateChanges").as[JsObject] should matchJson(expectedJson)
    }

    Get(routePath(s"/info?id=${invoke.id()}")) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsArray] \ 0 \ "stateChanges").as[JsObject] should matchJson(expectedJson)
    }

    Post(
      routePath("/info"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(invoke.id())).toString())
    ) ~> route ~> check {
      status shouldEqual StatusCodes.OK
      (responseAs[JsArray] \ 0 \ "stateChanges").as[JsObject] should matchJson(expectedJson)
    }
  }

  routePath("/address/{address}/limit/{limit}") - {
    val txByAddressLimit = settings.restAPISettings.transactionsByAddressLimit
    "handles parameter errors with corresponding responses" - {
      "invalid address bytes" in {
        Get(routePath(s"/address/${Base58.encode(new Array[Byte](24))}/limit/1")) ~> route should produce(InvalidAddress)
      }

      "invalid base58 encoding" in {
        Get(routePath(s"/address/${"1" * 23 + "0"}/limit/1")) ~> route should produce(
          CustomValidationError("requirement failed: Wrong char '0' in Base58 string '111111111111111111111110'")
        )
      }

      "invalid limit" - {
        "limit is too big" in {
          Get(
            routePath(s"/address/$richAddress/limit/${txByAddressLimit + 1}")
          ) ~> route should produce(TooBigArrayAllocation)
        }
      }

      "invalid after" in {
        val invalidBase58String = "1" * 23 + "0"
        Get(routePath(s"/address/$richAddress/limit/$txByAddressLimit?after=$invalidBase58String")) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
          (responseAs[JsObject] \ "message").as[String] shouldEqual s"Unable to decode transaction id $invalidBase58String"
        }
      }
    }

    "returns 200 if correct params provided" - {
      "address and limit" in {
        Get(routePath(s"/address/$richAddress/limit/$txByAddressLimit")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
        }
      }

      "address, limit and after" in {
        Get(routePath(s"/address/$richAddress/limit/$txByAddressLimit?after=${ByteStr(new Array[Byte](32))}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
        }
      }
    }

    "large-significand-format" in {
      val tx = TxHelpers.transfer(richAccount, TxHelpers.address(930), 10.waves)
      domain.appendBlock(tx)
      Get(routePath(s"/address/$richAddress/limit/1")) ~> Accept(CustomJson.jsonWithNumbersAsStrings) ~> route ~> check {
        val result = responseAs[JsArray] \ 0 \ 0
        (result \ "amount").as[String] shouldBe tx.amount.value.toString
        (result \ "fee").as[String] shouldBe tx.fee.value.toString

        (result \ "height").as[Int] shouldBe domain.blockchain.height
        (result \ "spentComplexity").as[Int] shouldBe 0
        (result \ "version").as[Int] shouldBe tx.version
        (result \ "type").as[Int] shouldBe tx.tpe.id
        (result \ "timestamp").as[Long] shouldBe tx.timestamp
      }
    }
  }

  routePath("/info/{id}") - {
    "returns meta for eth transfer" in {
      val ethAccount        = TxHelpers.signer(240).toEthKeyPair
      val transferRecipient = TxHelpers.address(241)

      val ethTransfer = EthTxGenerator.generateEthTransfer(ethAccount, transferRecipient, 5.waves, Asset.Waves)
      domain.appendBlock(
        TxHelpers.transfer(richAccount, ethAccount.toWavesAddress, 20.waves),
        ethTransfer
      )

      Get(routePath(s"/info/${ethTransfer.id()}")) ~> route ~> check {
        responseAs[JsObject] should matchJson(s"""{
                                                 |  "type" : 18,
                                                 |  "id" : "${ethTransfer.id()}",
                                                 |  "fee" : 100000,
                                                 |  "feeAssetId" : null,
                                                 |  "timestamp" : ${ethTransfer.timestamp},
                                                 |  "version" : 1,
                                                 |  "chainId" : 84,
                                                 |  "bytes" : "${EthEncoding.toHexString(ethTransfer.bytes())}",
                                                 |  "sender" : "${ethAccount.toWavesAddress}",
                                                 |  "senderPublicKey" : "${ethTransfer.sender}",
                                                 |  "height" : ${domain.blockchain.height},
                                                 |  "spentComplexity": 0,
                                                 |  "applicationStatus" : "succeeded",
                                                 |  "payload" : {
                                                 |    "type" : "transfer",
                                                 |    "recipient" : "$transferRecipient",
                                                 |    "asset" : null,
                                                 |    "amount" : ${5.waves}
                                                 |  }
                                                 |}""".stripMargin)
      }
    }

    "returns meta and state changes for eth invoke" in {
      val dapp   = TxHelpers.signer(245)
      val caller = TxHelpers.signer(246).toEthKeyPair

      val transaction = EthTxGenerator.generateEthInvoke(caller, dapp.toAddress, "test", Seq(EthTxGenerator.Arg.Integer(255)), Seq.empty)

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            dapp.toAddress        -> 1.waves,
            caller.toWavesAddress -> 1.waves
          ),
          fee = 0.002.waves
        ),
        TxHelpers.setScript(
          dapp,
          TestCompiler(V7).compileContract("""@Callable(i)
                                             |func test(arg: Int) = []
                                             |""".stripMargin)
        ),
        transaction
      )

      Get(routePath(s"/info/${transaction.id()}")) ~> route ~> check {
        responseAs[JsObject] should matchJson(s"""{
                                                 |  "type" : 18,
                                                 |  "id" : "${transaction.id()}",
                                                 |  "fee" : 500000,
                                                 |  "feeAssetId" : null,
                                                 |  "timestamp" : ${transaction.timestamp},
                                                 |  "version" : 1,
                                                 |  "chainId" : 84,
                                                 |  "bytes" : "${EthEncoding.toHexString(transaction.bytes())}",
                                                 |  "sender" : "${caller.toWavesAddress}",
                                                 |  "senderPublicKey" : "${transaction.sender}",
                                                 |  "height" : ${domain.blockchain.height},
                                                 |  "spentComplexity": 1,
                                                 |  "applicationStatus" : "succeeded",
                                                 |  "payload" : {
                                                 |    "type" : "invocation",
                                                 |    "dApp" : "${dapp.toAddress}",
                                                 |    "call" : {
                                                 |      "function" : "test",
                                                 |      "args" : [ {
                                                 |          "type" : "integer",
                                                 |          "value" : 255
                                                 |      } ]
                                                 |    },
                                                 |    "payment" : [ ],
                                                 |    "stateChanges" : {
                                                 |      "data" : [ ],
                                                 |      "transfers" : [ ],
                                                 |      "issues" : [ ],
                                                 |      "reissues" : [ ],
                                                 |      "burns" : [ ],
                                                 |      "sponsorFees" : [ ],
                                                 |      "leases" : [ ],
                                                 |      "leaseCancels" : [ ],
                                                 |      "invokes" : [ ]
                                                 |    }
                                                 |  }
                                                 |}""".stripMargin)
      }
    }

    "returns lease tx for lease cancel tx" in {
      val lessor         = TxHelpers.signer(250)
      val leaseRecipient = TxHelpers.address(251)

      val lease = TxHelpers.lease(lessor, leaseRecipient, 22.waves)

      domain.appendBlock(
        TxHelpers.transfer(richAccount, lessor.toAddress, 25.waves),
        lease
      )

      val leaseHeight = domain.blockchain.height

      val leaseCancel = TxHelpers.leaseCancel(lease.id(), lessor)
      domain.appendBlock(leaseCancel)
      val cancelHeight = domain.blockchain.height

      Get(routePath(s"/info/${leaseCancel.id()}")) ~> route ~> check {
        val json = responseAs[JsObject]
        json shouldBe Json.parse(s"""{
                                    |  "type" : 9,
                                    |  "id" : "${leaseCancel.id()}",
                                    |  "sender" : "${lessor.toAddress}",
                                    |  "senderPublicKey" : "${lessor.publicKey}",
                                    |  "fee" : 100000,
                                    |  "feeAssetId" : null,
                                    |  "timestamp" : ${leaseCancel.timestamp},
                                    |  "proofs" : [ "${leaseCancel.signature}" ],
                                    |  "version" : 2,
                                    |  "leaseId" : "${lease.id()}",
                                    |  "chainId" : 84,
                                    |  "height" : $cancelHeight,
                                    |  "applicationStatus" : "succeeded",
                                    |  "spentComplexity": 0,
                                    |  "lease" : {
                                    |    "id" : "${lease.id()}",
                                    |    "originTransactionId" : "${lease.id()}",
                                    |    "sender" : "${lessor.toAddress}",
                                    |    "recipient" : "$leaseRecipient",
                                    |    "amount" : ${22.waves},
                                    |    "height" : $leaseHeight,
                                    |    "status" : "canceled",
                                    |    "cancelHeight" : $cancelHeight,
                                    |    "cancelTransactionId" : "${leaseCancel.id()}"
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
      val height  = 0
      val tx      = TxHelpers.invoke()
      val succeed = true
      def validateResponse(): Unit = {
        status shouldEqual StatusCodes.OK

        val extraFields = Seq(
          if (domain.blockchain.isFeatureActivated(BF.BlockV5, height))
            Json.obj("applicationStatus" -> JsString(if (succeed) "succeeded" else "script_execution_failed"))
          else Json.obj(),
          Json.obj("height" -> height, "spentComplexity" -> 0)
        ).reduce(_ ++ _)

        responseAs[JsValue] should matchJson(tx.json() ++ extraFields)
      }

      Get(routePath(s"/info/${tx.id().toString}")) ~> route ~> check(validateResponse())
    }

    "handles multiple ids" in {
      val inputLimitErrMsg = TooBigArrayAllocation(transactionsApiRoute.settings.transactionsByAddressLimit).message
      val emptyInputErrMsg = "Transaction ID was not specified"

      val txCount = 5
      val txs     = (1 to txCount).map(_ => TxHelpers.invoke(TxHelpers.defaultSigner.toAddress))

      def checkResponse(txs: Seq[InvokeScriptTransaction]): Unit = txs.zip(responseAs[JsArray].value) foreach { case (tx, json) =>
        val extraFields =
          Json.obj("height" -> 1, "spentComplexity" -> 85, "applicationStatus" -> "succeeded", "stateChanges" -> InvokeScriptResult())
        json shouldBe (tx.json() ++ extraFields)
      }

      def checkErrorResponse(errMsg: String): Unit = {
        response.status shouldBe StatusCodes.BadRequest
        (responseAs[JsObject] \ "message").as[String] shouldBe errMsg
      }

      val maxLimitTxs      = Seq.fill(transactionsApiRoute.settings.transactionsByAddressLimit)(txs.head)
      val moreThanLimitTxs = txs.head +: maxLimitTxs

      Get(routePath(s"/info?${txs.map("id=" + _.id()).mkString("&")}")) ~> route ~> check(checkResponse(txs))
      Get(routePath(s"/info?${maxLimitTxs.map("id=" + _.id()).mkString("&")}")) ~> route ~> check(checkResponse(maxLimitTxs))
      Get(routePath(s"/info?${moreThanLimitTxs.map("id=" + _.id()).mkString("&")}")) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
      Get(routePath("/info")) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))

      Post(routePath("/info"), FormData(txs.map("id" -> _.id().toString)*)) ~> route ~> check(checkResponse(txs))
      Post(routePath("/info"), FormData(maxLimitTxs.map("id" -> _.id().toString)*)) ~> route ~> check(checkResponse(maxLimitTxs))
      Post(routePath("/info"), FormData(moreThanLimitTxs.map("id" -> _.id().toString)*)) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
      Post(routePath("/info"), FormData()) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))

      Post(
        routePath("/info"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(txs.map(_.id().toString: JsValueWrapper)*)).toString())
      ) ~> route ~> check(
        checkResponse(txs)
      )
      Post(
        routePath("/info"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(maxLimitTxs.map(_.id().toString: JsValueWrapper)*)).toString())
      ) ~> route ~> check(
        checkResponse(maxLimitTxs)
      )
      Post(
        routePath("/info"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(moreThanLimitTxs.map(_.id().toString: JsValueWrapper)*)).toString())
      ) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
      Post(
        routePath("/info"),
        HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> JsArray.empty).toString())
      ) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))
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
      val tx      = TxHelpers.invoke()
      val height  = 20
      val succeed = true

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
            if (domain.blockchain.isFeatureActivated(BF.BlockV5, height))
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

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      domain.utxPool.removeAll(domain.utxPool.all)
      val txs = Seq.tabulate(20)(a => TxHelpers.transfer(richAccount, amount = (a + 1).waves))
      txs.foreach(t => domain.utxPool.putIfNew(t))
      Get(routePath("/unconfirmed")) ~> route ~> check {
        val txIds = responseAs[Seq[JsValue]].map(v => (v \ "id").as[String])
        txIds should contain allElementsOf (txs.map(_.id().toString))
      }
      domain.utxPool.removeAll(txs)
    }

    routePath("/unconfirmed/size") - {
      "returns the size of unconfirmed transactions" in {
        domain.utxPool.removeAll(domain.utxPool.all)
        val txs = Seq.tabulate(20)(a => TxHelpers.transfer(richAccount, amount = (a + 1).waves))
        txs.foreach(t => domain.utxPool.putIfNew(t))
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
        domain.utxPool.removeAll(txs)
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
        val tx = TxHelpers.transfer(richAccount, defaultAddress, 20.waves)
        domain.utxPool.putIfNew(tx)
        Get(routePath(s"/unconfirmed/info/${tx.id().toString}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json()
        }
        domain.utxPool.removeAll(Seq(tx))
      }
    }

    routePath("/sign") - {
      "function call without args" in {
        val acc1 = domain.wallet.generateNewAccount().get
        val acc2 = domain.wallet.generateNewAccount().get

        val funcName          = "func"
        val funcWithoutArgs   = Json.obj("function" -> funcName)
        val funcWithEmptyArgs = Json.obj("function" -> funcName, "args" -> JsArray.empty)
        val funcWithArgs = InvokeScriptTxSerializer.functionCallToJson(
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
        val ist = Signed.invokeScript(
          TxVersion.V1,
          sender,
          sender.toAddress,
          None,
          Seq.empty,
          500000L,
          Asset.Waves,
          testTime.getTimestamp()
        )
        f(sender, ist)
      }

      "shows trace when trace is enabled" in {
        val sender = TxHelpers.signer(1201)
        val ist = TxHelpers.transfer(sender, defaultAddress, 1.waves)
        domain.appendBlock(
          TxHelpers.transfer(richAccount, sender.toAddress, 2.waves),
          TxHelpers.setScript(sender, TestCompiler(V7).compileExpression("throw(\"error\")"))
        )
        Post(routePath("/broadcast?trace=true"), ist.json()) ~> route ~> check {
          val result = responseAs[JsObject]
          (result \ "trace").as[JsValue] shouldBe Json.arr(AccountVerifierTrace(sender.toAddress,
            Some(ScriptExecutionError("error", List(
              "throw.@args" -> Right(ARR(IndexedSeq(CONST_STRING("error").explicitGet()), false).explicitGet()),
              "throw.@complexity" -> Right(CONST_LONG(1)),
              "@complexityLimit" -> Right(CONST_LONG(2147483646)),
            ), None))).json)
        }
      }

      "does not show trace when trace is disabled" in withInvokeScriptTransaction { (_, ist) =>
        Post(routePath("/broadcast"), ist.json()) ~> route ~> check {
          (responseAs[JsObject] \ "trace") shouldBe empty
        }
        Post(routePath("/broadcast?trace=false"), ist.json()) ~> route ~> check {
          (responseAs[JsObject] \ "trace") shouldBe empty
        }
      }

      "generates valid trace with vars" in {
        val sender     = TxHelpers.signer(1030)
        val aliasOwner = TxHelpers.signer(1031)
        val recipient  = TxHelpers.address(1032)

        val lease = TxHelpers.lease(sender, recipient, 50.waves)

        domain.appendBlock(
          TxHelpers.massTransfer(
            richAccount,
            Seq(
              sender.toAddress     -> 100.waves,
              aliasOwner.toAddress -> 1.waves
            ),
            fee = 0.002.waves
          ),
          TxHelpers.createAlias("test_alias", aliasOwner),
          TxHelpers.setScript(
            sender,
            TestCompiler(V5).compileContract(s"""{-# STDLIB_VERSION 5 #-}
                                                |{-# CONTENT_TYPE DAPP #-}
                                                |{-# SCRIPT_TYPE ACCOUNT #-}
                                                |
                                                |@Callable(i)
                                                |func default() = {
                                                |  let leaseToAddress = Lease(Address(base58'${recipient}'), ${10.waves})
                                                |  let leaseToAlias = Lease(Alias("test_alias"), ${20.waves})
                                                |  strict leaseId = leaseToAddress.calculateLeaseId()
                                                |
                                                |  [
                                                |    leaseToAddress,
                                                |    leaseToAlias,
                                                |    LeaseCancel(base58'${lease.id()}')
                                                |  ]
                                                |}
                                                |""".stripMargin)
          ),
          lease
        )

        val invoke = Signed
          .invokeScript(2.toByte, sender, sender.toAddress, None, Seq.empty, 0.005.waves, Asset.Waves, ntpTime.getTimestamp())

        Post(routePath("/broadcast?trace=true"), invoke.json()) ~> route ~> check {
          val dappTrace = (responseAs[JsObject] \ "trace").as[Seq[JsObject]].find(jsObject => (jsObject \ "type").as[String] == "dApp").get

          (dappTrace \ "error").get shouldEqual JsNull
          (dappTrace \ "vars" \\ "name").map(_.as[String]) should contain theSameElementsAs Seq(
            "i",
            "default.@args",
            "Address.@args",
            "Address.@complexity",
            "@complexityLimit",
            "Lease.@args",
            "Lease.@complexity",
            "@complexityLimit",
            "leaseToAddress",
            "calculateLeaseId.@args",
            "calculateLeaseId.@complexity",
            "@complexityLimit",
            "leaseId",
            "==.@args",
            "==.@complexity",
            "@complexityLimit",
            "Alias.@args",
            "Alias.@complexity",
            "@complexityLimit",
            "Lease.@args",
            "Lease.@complexity",
            "@complexityLimit",
            "leaseToAlias",
            "LeaseCancel.@args",
            "LeaseCancel.@complexity",
            "@complexityLimit",
            "cons.@args",
            "cons.@complexity",
            "@complexityLimit",
            "cons.@args",
            "cons.@complexity",
            "@complexityLimit",
            "cons.@args",
            "cons.@complexity",
            "@complexityLimit"
          )
        }
      }

      "checks the length of base58 attachment in symbols" in {
        val attachmentSizeInSymbols = TransferTransaction.MaxAttachmentStringSize + 1
        val attachmentStr           = "1" * attachmentSizeInSymbols

        val tx = TxHelpers
          .transfer()
          .copy(attachment = ByteStr(Base58.decode(attachmentStr))) // to bypass a validation
          .signWith(defaultSigner.privateKey)

        Post(routePath("/broadcast"), tx.json()) ~> route should produce(
          WrongJson(
            errors = Seq(
              JsPath \ "attachment" -> Seq(
                JsonValidationError(s"base58-encoded string length ($attachmentSizeInSymbols) exceeds maximum length of 192")
              )
            ),
            msg = Some("json data validation error, see validationErrors for details")
          )
        )
      }

      "checks the length of base58 attachment in bytes" in {
        val attachmentSizeInSymbols = TransferTransaction.MaxAttachmentSize + 1
        val attachmentStr           = "1" * attachmentSizeInSymbols
        val attachment              = ByteStr(Base58.decode(attachmentStr))

        val tx = TxHelpers
          .transfer()
          .copy(attachment = attachment)
          .signWith(defaultSigner.privateKey)

        Post(routePath("/broadcast"), tx.json()) ~> route should produce(
          TooBigInBytes(
            s"Invalid attachment. Length ${attachment.size} bytes exceeds maximum of ${TransferTransaction.MaxAttachmentSize} bytes."
          )
        )
      }
    }

    routePath("/merkleProof") - {
      def validateSuccess(expectedProofs: Seq[TransactionProof], response: HttpResponse): Unit = {
        response.status shouldBe StatusCodes.OK

        val proofs = responseAs[List[JsObject]]

        proofs.size shouldBe expectedProofs.size

        proofs.zip(expectedProofs).foreach { case (p, e) =>
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
        {
          val transactions = Seq.empty[Transaction]
          val proofs       = Seq.empty[TransactionProof]

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
        val genesisTransactions = domain.blocksApi.blockAtHeight(1).value._2.collect { case (_, tx) => tx.id() }

        val queryParams = genesisTransactions.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> genesisTransactions)

        Get(routePath(s"/merkleProof$queryParams")) ~> route ~> check {
          validateFailure(response)
        }

        Post(routePath("/merkleProof"), requestBody) ~> route ~> check {
          validateFailure(response)
        }
      }

      "handles invalid ids" in {
        val invalidIds = Seq(
          ByteStr.fill(AssetIdLength)(1),
          ByteStr.fill(AssetIdLength)(2)
        ).map(bs => s"${bs}0")

        Get(routePath(s"/merkleProof?${invalidIds.map("id=" + _).mkString("&")}")) ~> route should produce(InvalidIds(invalidIds))

        Post(routePath("/merkleProof"), FormData(invalidIds.map("id" -> _)*)) ~> route should produce(InvalidIds(invalidIds))

        Post(routePath("/merkleProof"), Json.obj("ids" -> invalidIds)) ~> route should produce(InvalidIds(invalidIds))
      }

      "handles transactions ids limit" in {
        val inputLimitErrMsg = TooBigArrayAllocation(transactionsApiRoute.settings.transactionsByAddressLimit).message
        val emptyInputErrMsg = "Transaction ID was not specified"

        def checkErrorResponse(errMsg: String): Unit = {
          response.status shouldBe StatusCodes.BadRequest
          (responseAs[JsObject] \ "message").as[String] shouldBe errMsg
        }

        def checkResponse(tx: TransferTransaction, idsCount: Int): Unit = {
          response.status shouldBe StatusCodes.OK

          val result = responseAs[JsArray].value
          result.size shouldBe idsCount
          (1 to idsCount).zip(responseAs[JsArray].value) foreach { case (_, json) =>
            (json \ "id").as[String] shouldBe tx.id().toString
            (json \ "transactionIndex").as[Int] shouldBe 1
          }
        }

        val sender = TxHelpers.signer(1090)

        val transferTx = TxHelpers.transfer(from = sender)
        domain.appendBlock(TxHelpers.transfer(richAccount, sender.toAddress, 100.waves), transferTx)

        val maxLimitIds      = Seq.fill(transactionsApiRoute.settings.transactionsByAddressLimit)(transferTx.id().toString)
        val moreThanLimitIds = transferTx.id().toString +: maxLimitIds

        Get(routePath(s"/merkleProof?${maxLimitIds.map("id=" + _).mkString("&")}")) ~> route ~> check(checkResponse(transferTx, maxLimitIds.size))
        Get(routePath(s"/merkleProof?${moreThanLimitIds.map("id=" + _).mkString("&")}")) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
        Get(routePath("/merkleProof")) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))

        Post(routePath("/merkleProof"), FormData(maxLimitIds.map("id" -> _)*)) ~> route ~> check(checkResponse(transferTx, maxLimitIds.size))
        Post(routePath("/merkleProof"), FormData(moreThanLimitIds.map("id" -> _)*)) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
        Post(routePath("/merkleProof"), FormData()) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))

        Post(
          routePath(s"/merkleProof"),
          HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(maxLimitIds.map(id => id: JsValueWrapper)*)).toString())
        ) ~> route ~> check(checkResponse(transferTx, maxLimitIds.size))
        Post(
          routePath(s"/merkleProof"),
          HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(moreThanLimitIds.map(id => id: JsValueWrapper)*)).toString())
        ) ~> route ~> check(checkErrorResponse(inputLimitErrMsg))
        Post(
          routePath("/merkleProof"),
          HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> JsArray.empty).toString())
        ) ~> route ~> check(checkErrorResponse(emptyInputErrMsg))
      }
    }

    "NODE-969. Transactions API should return correct data for orders with attachment" in {
      def checkOrderAttachment(txInfo: JsObject, expectedAttachment: ByteStr): Assertion = {
        implicit val byteStrFormat: Format[ByteStr] = com.wavesplatform.utils.byteStrFormat
        (txInfo \ "order1" \ "attachment").asOpt[ByteStr] shouldBe Some(expectedAttachment)
      }

      val sender     = TxHelpers.signer(1100)
      val issuer     = TxHelpers.signer(1101)
      val attachment = ByteStr.fill(32)(1)
      val issue      = TxHelpers.issue(issuer)
      val exchange =
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, Waves, issue.asset, version = Order.V4, attachment = Some(attachment)),
          TxHelpers.order(OrderType.SELL, Waves, issue.asset, version = Order.V4, sender = issuer),
          version = TxVersion.V3
        )

      domain.appendBlock(
        TxHelpers.massTransfer(richAccount, Seq(sender.toAddress -> 10.waves, issuer.toAddress -> 10.waves), fee = 0.002.waves),
        issue,
        exchange
      )

      domain.liquidAndSolidAssert { () =>
        Get(s"/transactions/info/${exchange.id()}") ~> route ~> check {
          checkOrderAttachment(responseAs[JsObject], attachment)
        }

        Post("/transactions/info", FormData("id" -> exchange.id().toString)) ~> route ~> check {
          checkOrderAttachment(responseAs[JsArray].value.head.as[JsObject], attachment)
        }

        Post(
          "/transactions/info",
          HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(exchange.id().toString)).toString())
        ) ~> route ~> check {
          checkOrderAttachment(responseAs[JsArray].value.head.as[JsObject], attachment)
        }

        Get(s"/transactions/address/${exchange.sender.toAddress}/limit/10") ~> route ~> check {
          checkOrderAttachment(responseAs[JsArray].value.head.as[JsArray].value.head.as[JsObject], attachment)
        }
      }
    }
  }
}
