package com.wavesplatform.it.sync.transactions

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.{Mistiming, StateCheckFailed, WrongJson}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{script, someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount}
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.Random

class SetAssetScriptTransactionSuite extends BaseTransactionSuite {
  val estimator = ScriptEstimatorV1

  var assetWOScript    = ""
  var assetWScript     = ""
  var assetWScript2    = ""
  private def accountB = secondKeyPair
  private val unchangeableScript = ScriptCompiler(
    s"""match tx {
       |  case _: SetAssetScriptTransaction => false
       |  case _ => true
       |}
       |""".stripMargin,
    isAssetScript = true,
    estimator
  ).explicitGet()._1

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetWOScript = sender
      .issue(
        firstKeyPair,
        "AssetWOScript",
        "Test coin for SetAssetScript tests w/o script",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        version = 2.toByte
      )
      .id

    assetWScript = sender
      .issue(
        firstKeyPair,
        "SetAssetScript",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2.toByte,
        Some(scriptBase64)
      )
      .id

    assetWScript2 = sender
      .issue(
        firstKeyPair,
        "SetAssetScript",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2.toByte,
        Some(scriptBase64),
        waitForTx = true
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(assetWOScript)
    nodes.waitForHeightAriseAndTxPresent(assetWScript)
    sender.postForm("/addresses")
    sender.postForm("/addresses")
    sender.postForm("/addresses")
  }

  test("issuer cannot change script on asset w/o initial script") {
    for (v <- setAssetScrTxSupportedVersions) {
      val (balance, eff) = miner.accountBalances(firstAddress)
      assertApiError(
        sender.setAssetScript(assetWOScript, firstKeyPair, setAssetScriptFee, Some(scriptBase64), version = v),
        AssertiveApiError(StateCheckFailed.Id, StateCheckFailed.message("Cannot set script on an asset issued without a script"))
      )
      assertApiError(
        broadcastSetAssetScriptJson(assetWOScript, firstKeyPair, setAssetScriptFee, version = v),
        AssertiveApiError(WrongJson.Id, WrongJson.WrongJsonDataMessage, matchMessage = true)
      )
      assertApiError(
        broadcastSetAssetScriptJson(assetWOScript, firstKeyPair, setAssetScriptFee, Some(""), version = v),
        AssertiveApiError(WrongJson.Id, WrongJson.WrongJsonDataMessage, matchMessage = true)
      )
      miner.assertBalances(firstAddress, balance, eff)
    }
  }

  private def broadcastSetAssetScriptJson(assetId: String, issuer: KeyPair, fee: Long, script: Option[String] = None, version: Byte) =
    sender.signedBroadcast(
      Json.obj(
        "type"            -> SetAssetScriptTransaction.typeId,
        "version"         -> version,
        "assetId"         -> assetId,
        "sender"          -> issuer.toAddress,
        "senderPublicKey" -> issuer.publicKey,
        "fee"             -> fee,
        "script"          -> script.fold[JsValue](JsNull)(JsString),
        "timestamp"       -> System.currentTimeMillis()
      )
    )

  test("non-issuer cannot change script") {
    /*
    issuer is first address, but script allows make SetAssetScript only second address
     */
    val assetWAnotherOwner = sender
      .issue(
        firstKeyPair,
        "NonOwnCoin",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2.toByte,
        script = Some(
          ScriptCompiler(
            s"""match tx {
               |case s : SetAssetScriptTransaction => s.sender == addressFromPublicKey(base58'${secondKeyPair.publicKey}')
               |case _ => false
               |}
               |""".stripMargin,
            isAssetScript = true,
            estimator
          ).explicitGet()._1.bytes().base64
        )
      )
      .id
    nodes.waitForHeightAriseAndTxPresent(assetWAnotherOwner)

    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(sender.setAssetScript(assetWAnotherOwner, secondKeyPair, setAssetScriptFee, Some(scriptBase64), version = v)) { error =>
        error.id shouldBe StateCheckFailed.Id
        error.message shouldBe StateCheckFailed.message("Asset was issued by other address")
      }
      assertApiError(broadcastSetAssetScriptJson(assetWAnotherOwner, secondKeyPair, setAssetScriptFee, Some(""), version = v)) { error =>
        error.id shouldBe WrongJson.Id
        error.message shouldBe WrongJson.WrongJsonDataMessage
      }
    }
  }

  test("non-issuer cannot change script on asset w/o script") {
    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val (balance2, eff2) = miner.accountBalances(secondAddress)
    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(sender.setAssetScript(assetWOScript, secondKeyPair, setAssetScriptFee, Some(scriptBase64), version = v)) { error =>
        error.id shouldBe StateCheckFailed.Id
        error.message shouldBe StateCheckFailed.message("Asset was issued by other address")
      }

      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("sender's waves balance is decreased by fee") {
    val script2 = ScriptCompiler(
      s"""
         |match tx {
         |  case _: SetAssetScriptTransaction => true
         |  case _ => false
         |}
         """.stripMargin,
      isAssetScript = true,
      estimator
    ).explicitGet()._1.bytes().base64

    val details = miner.assetsDetails(assetWScript, true).scriptDetails.getOrElse(fail("Expecting to get asset details"))
    assert(details.scriptComplexity == 1)
    assert(details.scriptText == "true")
    assert(details.script == scriptBase64)
    for (v <- setAssetScrTxSupportedVersions) {
      val (balance, eff) = miner.accountBalances(firstAddress)
      val txId           = sender.setAssetScript(assetWScript, firstKeyPair, setAssetScriptFee, Some(script2), version = v).id
      nodes.waitForHeightAriseAndTxPresent(txId)
      miner.assertBalances(firstAddress, balance - setAssetScriptFee, eff - setAssetScriptFee)
      val details2 = miner.assetsDetails(assetWScript, true).scriptDetails.getOrElse(fail("Expecting to get asset details"))
      assert(details2.scriptComplexity == 6)
      assert(details2.script == script2)
    }
  }

  test("cannot transact without having enough waves") {
    val (balance, eff) = miner.accountBalances(firstAddress)
    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(sender.setAssetScript(assetWScript, firstKeyPair, balance + 1, Some(scriptBase64), version = v)) { error =>
        error.id shouldBe StateCheckFailed.Id
        error.message shouldBe StateCheckFailed.message("Accounts balance errors")
      }
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance, eff)
    }

    val leaseAmount = 1.waves
    val leaseId     = sender.lease(firstKeyPair, secondAddress, leaseAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseId)

    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(sender.setAssetScript(assetWScript, firstKeyPair, balance - leaseAmount, Some(scriptBase64), version = v)) { error =>
        error.id shouldBe StateCheckFailed.Id
        error.message should include regex StateCheckFailed.message(s"Accounts balance errors")
      }
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance - minFee, eff - leaseAmount - minFee)
    }
  }

  test("invalid transaction should not be in UTX or blockchain") {
    for (v <- setAssetScrTxSupportedVersions) {
      def sastx(
          fee: Long = setAssetScriptFee,
          timestamp: Long = System.currentTimeMillis,
          assetId: IssuedAsset = IssuedAsset(ByteStr.decodeBase58(assetWScript).get)
      ): SetAssetScriptTransaction =
        SetAssetScriptTransaction
          .signed(version = v, sender.keyPair.publicKey, assetId, Some(script), fee, timestamp, sender.keyPair.privateKey)
          .explicitGet()

      val (balance, eff) = miner.accountBalances(firstAddress)

      val invalidTxs = Seq(
        (
          sastx(timestamp = System.currentTimeMillis + 1.day.toMillis),
          Mistiming("Transaction timestamp .* is more than .*ms in the future").assertive(true)
        ),
        (
          sastx(fee = 9999999),
          AssertiveApiError(StateCheckFailed.Id, "Fee .* does not exceed minimal value", StateCheckFailed.Code, true)
        ),
        (
          sastx(assetId = IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz9ekQuYn92natMnMq8").get)),
          AssertiveApiError(WrongJson.Id, WrongJson.WrongJsonDataMessage, StatusCodes.BadRequest, true)
        ),
        (
          sastx(assetId = IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get)),
          AssertiveApiError(StateCheckFailed.Id, "Referenced assetId not found", StateCheckFailed.Code, true)
        )
      )

      for ((tx, diag) <- invalidTxs) {
        assertApiError(sender.broadcastRequest(tx.json()), diag)
        nodes.foreach(_.ensureTxDoesntExist(tx.id().toString))
      }

      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance, eff)
    }
  }

  test("transaction requires a valid proof") {
    for (v <- setAssetScrTxSupportedVersions) {
      val request: JsObject =
        SetAssetScriptTransaction
          .selfSigned(
            v,
            firstKeyPair,
            IssuedAsset(ByteStr.decodeBase58(assetWScript).get),
            Some(script),
            setAssetScriptFee,
            System.currentTimeMillis()
          )
          .explicitGet()
          .json()

      def id(obj: JsObject) = obj.value("id").as[String]

      val noProof = request - "proofs"
      assertApiError(sender.postJson("/transactions/broadcast", noProof)) { error =>
        error.message should include regex WrongJson.WrongJsonDataMessage

        val validationErrors = (error.json \ "validationErrors" \ "obj.proofs").as[JsArray].value.flatMap(json => (json \ "msg").as[List[String]])
        validationErrors should contain("error.path.missing")
      }
      nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

      val badProof = request ++ Json.obj("proofs" -> Seq(Base58.encode(Array.fill(64)(Random.nextInt().toByte))))
      assertApiError(sender.postJson("/transactions/broadcast", badProof)) { error =>
        error.message should include regex "Proof doesn't validate as signature"
      }
      nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

      val withProof = request
      assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
      sender.postJson("/transactions/broadcast", withProof)
      nodes.waitForHeightAriseAndTxPresent(id(withProof))
    }
  }

  test("try to update script to null") {
    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(broadcastSetAssetScriptJson(assetWScript, firstKeyPair, setAssetScriptFee, version = v)) { error =>
        error.message should include regex WrongJson.WrongJsonDataMessage
      }
      assertApiError(broadcastSetAssetScriptJson(assetWScript, firstKeyPair, setAssetScriptFee, Some(""), version = v)) { error =>
        error.message should include regex WrongJson.WrongJsonDataMessage
      }
    }
  }

  test("try to make SetAssetScript tx on script that deprecates SetAssetScript") {
    /*
    script doesn't allow do SetAssetScript
     */
    val assetUnchangeableScript = sender
      .issue(
        firstKeyPair,
        "SetAssetWDep",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2.toByte,
        script = Some(unchangeableScript.bytes().base64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(assetUnchangeableScript)

    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(sender.setAssetScript(assetUnchangeableScript, firstKeyPair, setAssetScriptFee, Some(scriptBase64), version = v)) { error =>
        error.message shouldBe errNotAllowedByToken
      }
    }
  }

  test("non-issuer can change script if issuer's account script allows (proof correct)") {
    val accountA = firstKeyPair

    for (v <- setAssetScrTxSupportedVersions) {
      val assetWithScript = if (v < 2) assetWScript else assetWScript2
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(
          version = v,
          accountA,
          Some(
            ScriptCompiler(
              s"""|let pkB = base58'${accountB.publicKey}'
                  |match tx {
                  |  case s: SetAssetScriptTransaction => sigVerify(s.bodyBytes,s.proofs[0],pkB)
                  |  case _ => true
                  |}
                """.stripMargin,
              isAssetScript = false,
              estimator
            ).explicitGet()._1
          ),
          setScriptFee + smartFee,
          System.currentTimeMillis()
        )
        .explicitGet()

      val setScriptId = sender
        .signedBroadcast(setScriptTransaction.json())
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)

      val nonIssuerUnsignedTx = SetAssetScriptTransaction(
        version = v,
        accountA.publicKey,
        IssuedAsset(ByteStr.decodeBase58(assetWithScript).get),
        Some(unchangeableScript),
        TxPositiveAmount.unsafeFrom(setAssetScriptFee + smartFee),
        System.currentTimeMillis,
        Proofs.empty,
        accountA.toAddress.chainId
      )

      val sigTxB = crypto.sign(accountB.privateKey, nonIssuerUnsignedTx.bodyBytes())

      val signedTxByB =
        nonIssuerUnsignedTx.copy(proofs = Proofs(Seq(sigTxB)))

      val tx =
        sender.signedBroadcast(signedTxByB.json()).id

      nodes.waitForHeightAriseAndTxPresent(tx)

      // try to change unchangeable script
      val nonIssuerUnsignedTx2 = SetAssetScriptTransaction(
        version = v,
        accountA.publicKey,
        IssuedAsset(ByteStr.decodeBase58(assetWithScript).get),
        Some(script),
        TxPositiveAmount.unsafeFrom(setAssetScriptFee + smartFee),
        System.currentTimeMillis,
        Proofs.empty,
        accountA.toAddress.chainId
      )

      val sigTxB2 = crypto.sign(accountB.privateKey, nonIssuerUnsignedTx2.bodyBytes())

      val signedTxByB2 =
        nonIssuerUnsignedTx2.copy(proofs = Proofs(Seq(sigTxB2)))

      assertApiError(sender.signedBroadcast(signedTxByB2.json())) { error =>
        error.message shouldBe errNotAllowedByToken
      }
      nodes.foreach(_.ensureTxDoesntExist(signedTxByB2.id().toString))
    }
  }

  test("try to make SetAssetScript for asset v1") {
    val assetV1 = sender
      .issue(thirdKeyPair, "AssetV1", "Test coin for V1", someAssetAmount, 0, reissuable = false, issueFee)
      .id
    nodes.waitForHeightAriseAndTxPresent(assetV1)

    val (balance1, eff1) = miner.accountBalances(thirdAddress)
    val (balance2, eff2) = miner.accountBalances(secondAddress)
    for (v <- setAssetScrTxSupportedVersions) {
      assertApiError(sender.setAssetScript(assetV1, thirdKeyPair, setAssetScriptFee, Some(scriptBase64), version = v)) { error =>
        error.message.contains("Reason: Cannot set script on an asset issued without a script") shouldBe true
      }

      assertApiError(sender.setAssetScript(assetV1, secondKeyPair, setAssetScriptFee, Some(scriptBase64), version = v)) { error =>
        error.message.contains("Reason: Asset was issued by other address") shouldBe true
      }
    }

    miner.assertBalances(thirdAddress, balance1, eff1)
    miner.assertBalances(secondAddress, balance2, eff2)

  }

}
