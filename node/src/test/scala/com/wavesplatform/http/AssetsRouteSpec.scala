package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.api.http.requests.{TransferV1Request, TransferV2Request}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.diffs.ci
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, BinaryDataEntry, Diff, Height}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.utils.EthTxGenerator.Arg
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxVersion}
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{RequestGen, TestValues, TestWallet}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.Assertion
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

class AssetsRouteSpec
    extends RouteSpec("/assets")
    with RequestGen
    with Eventually
    with PathMockFactory
    with RestAPISettingsHelper
    with WithDomain
    with TestWallet {

  private val wallet = stub[Wallet]

  private def route(domain: Domain) =
    AssetsApiRoute(
      restAPISettings,
      wallet,
      DummyTransactionPublisher.accepting,
      domain.blockchain,
      ntpTime,
      CommonAccountsApi(() => domain.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), domain.db, domain.blockchain),
      CommonAssetsApi(() => domain.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), domain.db, domain.blockchain),
      MaxDistributionDepth
    )

  private def withRoute(f: (Domain, Route) => Unit): Unit =
    withDomain(domainSettingsWithPreactivatedFeatures(BlockchainFeatures.implemented.flatMap(BlockchainFeatures.feature).toSeq: _*)) { d =>
      f(d, route(d).route)
    }

  private val seed                 = "seed".getBytes("UTF-8")
  private val senderPrivateKey     = Wallet.generateNewAccount(seed, 0)
  private val receiverPrivateKey   = Wallet.generateNewAccount(seed, 1)
  private val MaxDistributionDepth = 1

  private def setScriptTransaction(sender: KeyPair) =
    SetScriptTransaction
      .selfSigned(
        TxVersion.V2,
        sender,
        Some(
          TestCompiler(V6).compileContract(
            """
              |{-# STDLIB_VERSION 6 #-}
              |{-# CONTENT_TYPE DAPP #-}
              |{-# SCRIPT_TYPE ACCOUNT #-}
              |
              |@Callable(inv)
              |func issue(name: String, description: String, amount: Int, decimals: Int, isReissuable: Boolean) = {
              |  let t = Issue(name, description, amount, decimals, isReissuable)
              |  [
              |    t,
              |    BinaryEntry("assetId", calculateAssetId(t))
              |  ]
              |}
              |""".stripMargin
          )
        ),
        0.01.waves,
        ntpTime.getTimestamp()
      )
      .explicitGet()

  private def issueTransaction(name: Option[String] = None, script: Option[Script] = None, quantity: Option[Long] = None): IssueTransaction =
    IssueTransaction
      .selfSigned(
        version = TxVersion.V2,
        sender = defaultSigner,
        name = name.getOrElse(assetDesc.name.toStringUtf8),
        description = assetDesc.description.toStringUtf8,
        quantity = quantity.getOrElse(assetDesc.totalVolume.toLong),
        decimals = assetDesc.decimals.toByte,
        reissuable = assetDesc.reissuable,
        script = script,
        fee = 1.waves,
        timestamp = TxHelpers.timestamp
      )
      .explicitGet()

  private val assetDesc = AssetDescription(
    ByteStr.empty,
    issuer = TxHelpers.defaultSigner.publicKey,
    name = ByteString.copyFromUtf8("test"),
    description = ByteString.copyFromUtf8("desc"),
    decimals = 0,
    reissuable = true,
    totalVolume = 1,
    lastUpdatedAt = Height(1),
    script = None,
    sponsorship = 0,
    nft = false
  )

  "/balance/{address}" - {
    "multiple ids" in {
      withRoute { (d, route) =>
        val issueTx1 = issueTransaction(name = Some("aaaa"), quantity = Some(100))
        val issueTx2 = issueTransaction(name = Some("bbbb"), quantity = Some(100))

        d.appendBlock(TxHelpers.genesis(defaultSigner.toAddress))
        d.appendBlock(issueTx1, issueTx2)

        route.anyParamTest(routePath(s"/balance/${TxHelpers.defaultAddress}"), "id")(issueTx1.id().toString, issueTx2.id().toString) {
          status shouldBe StatusCodes.OK
          responseAs[JsValue] should matchJson(s"""{
                                                  |  "address" : "${defaultSigner.toAddress}",
                                                  |  "balances" : [ {
                                                  |    "assetId" : "${issueTx1.id()}",
                                                  |    "reissuable" : ${issueTx1.reissuable},
                                                  |    "minSponsoredAssetFee" : null,
                                                  |    "sponsorBalance" : null,
                                                  |    "quantity" : ${issueTx1.quantity},
                                                  |    "issueTransaction" : ${issueTx1.json()},
                                                  |    "balance" : ${issueTx1.quantity}
                                                  |  }, {
                                                  |    "assetId" : "${issueTx2.id()}",
                                                  |    "reissuable" : ${issueTx2.reissuable},
                                                  |    "minSponsoredAssetFee" : null,
                                                  |    "sponsorBalance" : null,
                                                  |    "quantity" : ${issueTx2.quantity},
                                                  |    "issueTransaction" : ${issueTx2.json()},
                                                  |    "balance" : ${issueTx2.quantity}
                                                  |  } ]
                                                  |}
                                                  |""".stripMargin)
        }

        route.anyParamTest(routePath(s"/balance/${TxHelpers.defaultAddress}"), "id")("____", "----") {
          status shouldBe StatusCodes.BadRequest
          responseAs[JsValue] should matchJson("""{
                                                 |    "error": 116,
                                                 |    "message": "Request contains invalid IDs. ____, ----",
                                                 |    "ids": [
                                                 |        "____",
                                                 |        "----"
                                                 |    ]
                                                 |}""".stripMargin)
        }

        withClue("over limit")(route.anyParamTest(routePath(s"/balance/${TxHelpers.defaultAddress}"), "id")(Seq.fill(101)("aaa"): _*) {
          status shouldBe StatusCodes.BadRequest
          responseAs[JsValue] should matchJson("""{
                                                 |  "error" : 10,
                                                 |  "message" : "Too big sequence requested: max limit is 100 entries"
                                                 |}""".stripMargin)
        })

        withClue("old GET portfolio")(Get(routePath(s"/balance/${TxHelpers.defaultAddress}")) ~> route ~> check { // portfolio
          status shouldBe StatusCodes.OK
          responseAs[JsValue] should matchJson(s"""{
                                                  |  "address" : "${defaultSigner.toAddress}",
                                                  |  "balances" : [ {
                                                  |    "assetId" : "${issueTx2.id()}",
                                                  |    "reissuable" : ${issueTx2.reissuable},
                                                  |    "minSponsoredAssetFee" : null,
                                                  |    "sponsorBalance" : null,
                                                  |    "quantity" : ${issueTx2.quantity},
                                                  |    "issueTransaction" : ${issueTx2.json()},
                                                  |    "balance" : ${issueTx2.quantity}
                                                  |  }, {
                                                  |    "assetId" : "${issueTx1.id()}",
                                                  |    "reissuable" : ${issueTx1.reissuable},
                                                  |    "minSponsoredAssetFee" : null,
                                                  |    "sponsorBalance" : null,
                                                  |    "quantity" : ${issueTx1.quantity},
                                                  |    "issueTransaction" : ${issueTx1.json()},
                                                  |    "balance" : ${issueTx1.quantity}
                                                  |  } ]
                                                  |}
                                                  |""".stripMargin)
        })
      }
    }
  }

  "/transfer" - {
    withRoute { (_, route) =>
      def posting[A: Writes](v: A): RouteTestResult =
        Post(routePath("/transfer"), v).addHeader(ApiKeyHeader) ~> route

      (wallet.privateKeyAccount _).when(senderPrivateKey.toAddress).onCall((_: Address) => Right(senderPrivateKey)).anyNumberOfTimes()

      "accepts TransferRequest" in {
        val req = TransferV1Request(
          assetId = None,
          feeAssetId = None,
          amount = 1.waves,
          fee = 0.3.waves,
          sender = senderPrivateKey.toAddress.toString,
          attachment = Some("attachment"),
          recipient = receiverPrivateKey.toAddress.toString,
          timestamp = Some(System.currentTimeMillis())
        )

        posting(req) ~> check {
          status shouldBe StatusCodes.OK
          responseAs[TransferTransaction]
        }
      }

      "accepts VersionedTransferRequest" in {
        val req = TransferV2Request(
          assetId = None,
          amount = 1.waves,
          feeAssetId = None,
          fee = 0.3.waves,
          sender = senderPrivateKey.toAddress.toString,
          attachment = None,
          recipient = receiverPrivateKey.toAddress.toString,
          timestamp = Some(System.currentTimeMillis())
        )

        posting(req) ~> check {
          status shouldBe StatusCodes.OK
          responseAs[TransferV2Request]
        }
      }

      "returns a error if it is not a transfer request" in {
        val req = issueReq.sample.get
        posting(req) ~> check {
          status shouldNot be(StatusCodes.OK)
        }
      }
    }
  }

  routePath(s"/details/{id} - issued by invoke expression") in {
    withRoute { (d, route) =>
      val tx = TxHelpers.invokeExpression(
        expression = TestCompiler(V6).compileFreeCall(
          s"""
             |let t = Issue("${assetDesc.name.toStringUtf8}", "${assetDesc.description.toStringUtf8}", ${assetDesc.totalVolume}, ${assetDesc.decimals}, ${assetDesc.reissuable})
             |[
             |  t,
             |  BinaryEntry("assetId", calculateAssetId(t))
             |]""".stripMargin
        ),
        fee = 1.01.waves
      )

      d.appendBlock(TxHelpers.genesis(tx.sender.toAddress))
      d.appendBlock(tx)

      val assetId = d.blockchain
        .accountData(tx.sender.toAddress, "assetId")
        .collect { case i: BinaryDataEntry =>
          i.value
        }
        .get

      d.liquidAndSolidAssert { () =>
        checkDetails(d, route, tx, assetId.toString, assetDesc)
      }
    }
  }

  routePath(s"/details/{id} - issued by Ethereum transaction") in {
    val setScript = setScriptTransaction(defaultSigner)
    val invoker   = TxHelpers.defaultEthSigner
    val fee       = 1.01.waves

    Seq(true, false)
      .foreach { useInvokeExpression =>
        withRoute { (d, route) =>
          val tx =
            if (useInvokeExpression) {
              val args =
                List(
                  CONST_STRING(assetDesc.name.toStringUtf8).explicitGet(),
                  CONST_STRING(assetDesc.description.toStringUtf8).explicitGet(),
                  CONST_LONG(assetDesc.totalVolume.toInt),
                  CONST_LONG(assetDesc.decimals),
                  CONST_BOOLEAN(assetDesc.reissuable)
                )
              ci.toEthInvokeExpression(setScript, invoker, "issue", args, fee)
            } else {
              EthTxGenerator.generateEthInvoke(
                keyPair = invoker,
                address = defaultSigner.toAddress,
                funcName = "issue",
                args = Seq(
                  Arg.Str(assetDesc.name.toStringUtf8),
                  Arg.Str(assetDesc.description.toStringUtf8),
                  Arg.Integer(assetDesc.totalVolume.toInt),
                  Arg.Integer(assetDesc.decimals),
                  Arg.Bool(assetDesc.reissuable)
                ),
                payments = Seq.empty,
                fee
              )
            }
          val description = if (useInvokeExpression) assetDesc.copy(issuer = tx.sender) else assetDesc
          val dataAddress = if (useInvokeExpression) tx.sender.toAddress else defaultSigner.toAddress

          d.appendBlock(TxHelpers.genesis(tx.sender.toAddress), TxHelpers.genesis(defaultSigner.toAddress))
          d.appendBlock(setScript, tx)

          val assetId = d.blockchain
            .accountData(dataAddress, "assetId")
            .collect { case i: BinaryDataEntry => i.value }
            .get

          d.liquidAndSolidAssert { () =>
            checkDetails(d, route, tx, assetId.toString, description)
          }
        }
      }
  }

  private val smartIssueAndDetailsGen = {
    scriptGen.map { script =>
      (
        issueTransaction(script = Some(script)),
        assetDesc.copy(script =
          Some(
            AssetScriptInfo(
              script,
              Script.estimate(script, ScriptEstimatorV1, fixEstimateOfVerifier = true, useContractVerifierLimit = false).explicitGet()
            )
          )
        )
      )
    }
  }

  routePath(s"/details/{id} - smart asset") in forAll(smartIssueAndDetailsGen) { case (tx, assetDesc) =>
    withRoute { (d, route) =>
      d.appendBlock(TxHelpers.genesis(tx.sender.toAddress))
      d.appendBlock(tx)

      d.liquidAndSolidAssert { () =>
        checkDetails(d, route, tx, tx.id().toString, assetDesc)
      }
    }
  }

  routePath(s"/details/{id} - non-smart asset") in {
    withRoute { (d, route) =>
      val tx = issueTransaction()

      d.appendBlock(TxHelpers.genesis(tx.sender.toAddress))
      d.appendBlock(tx)

      d.liquidAndSolidAssert { () =>
        checkDetails(d, route, tx, tx.id().toString, assetDesc)
      }
    }
  }

  routePath("/{assetId}/distribution/{height}/limit/{limit}") in {
    withRoute { (d, route) =>
      val tx = issueTransaction()

      d.appendBlock(TxHelpers.genesis(tx.sender.toAddress))
      d.appendBlock(tx)
      d.appendBlock()

      Get(routePath(s"/${tx.id()}/distribution/2/limit/$MaxAddressesPerRequest")) ~> route ~> check {
        val response = responseAs[JsObject]
        response shouldBe Json.obj(
          "hasNext"  -> false,
          "lastItem" -> tx.sender.toAddress.toString,
          "items" -> Json.obj(
            tx.sender.toAddress.toString -> 1L
          )
        )
      }

      Get(routePath(s"/${TestValues.asset.id}/distribution/2/limit/${MaxAddressesPerRequest + 1}")) ~> route ~> check {
        responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Limit should be less than or equal to $MaxAddressesPerRequest")
      }

      Get(routePath(s"/${TestValues.asset.id}/distribution/1/limit/1")) ~> route ~> check {
        responseAs[JsObject] shouldBe Json.obj(
          "error"   -> 199,
          "message" -> s"Unable to get distribution past height ${d.blockchain.height - MaxDistributionDepth}"
        )
      }
    }
  }

  private def checkDetails(domain: Domain, route: Route, tx: Transaction, assetId: String, assetDesc: AssetDescription): Unit = {
    domain.liquidAndSolidAssert { () =>
      Get(routePath(s"/details/$assetId")) ~> route ~> check {
        val response = responseAs[JsObject]
        checkResponse(tx, assetDesc, assetId, response)
      }
      Get(routePath(s"/details?id=$assetId")) ~> route ~> check {
        val responses = responseAs[List[JsObject]]
        responses.foreach(response => checkResponse(tx, assetDesc, assetId, response))
      }
      Post(routePath("/details"), Json.obj("ids" -> List(s"$assetId"))) ~> route ~> check {
        val responses = responseAs[List[JsObject]]
        responses.foreach(response => checkResponse(tx, assetDesc, assetId, response))
      }
    }
  }

  private def checkResponse(tx: Transaction, desc: AssetDescription, assetId: String, response: JsObject): Assertion = {
    response should matchJson(
      s"""
         |{
         |  "assetId": "$assetId",
         |  "issueHeight": 2,
         |  "issueTimestamp": ${tx.timestamp},
         |  "issuer": "${desc.issuer.toAddress.toString}",
         |  "issuerPublicKey": "${desc.issuer.toString}",
         |  "name": "${desc.name.toStringUtf8}",
         |  "description": "${desc.description.toStringUtf8}",
         |  "decimals": ${desc.decimals},
         |  "reissuable": ${desc.reissuable},
         |  "quantity": ${desc.totalVolume},
         |  "scripted": ${desc.script.isDefined},
         |  "minSponsoredAssetFee": null,
         |  "originTransactionId": "${tx.id().toString}"
         |}
         |""".stripMargin
    )
  }
}
