package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{script, someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import play.api.libs.json._
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.util.Random

class SetAssetScriptTransactionSuite extends BaseTransactionSuite {
  var assetWOScript    = ""
  var assetWScript     = ""
  private val accountB = pkByAddress(secondAddress)
  private val unchangeableScript = ScriptCompiler(
    s"""
                                               |match tx {
                                               |case s : SetAssetScriptTransaction => false
                                               |case _ => true}""".stripMargin,
    isAssetScript = true
  ).explicitGet()._1

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetWOScript = sender
      .issue(firstAddress, "AssetWOScript", "Test coin for SetAssetScript tests w/o script", someAssetAmount, 0, reissuable = false, issueFee, 2)
      .id

    assetWScript = sender
      .issue(
        firstAddress,
        "SetAssetScript",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        Some(scriptBase64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(assetWOScript)
    nodes.waitForHeightAriseAndTxPresent(assetWScript)
  }

  test("issuer cannot change script on asset w/o initial script") {
    val (balance, eff) = notMiner.accountBalances(firstAddress)
    assertBadRequestAndMessage(
      sender.setAssetScript(assetWOScript, firstAddress, setAssetScriptFee, Some(scriptBase64)),
      "Reason: Cannot set script on an asset issued without a script"
    )
    assertBadRequestAndMessage(sender.setAssetScript(assetWOScript, firstAddress, setAssetScriptFee), "Reason: Cannot set empty script")

    assertBadRequestAndMessage(
      sender.setAssetScript(assetWOScript, firstAddress, setAssetScriptFee, Some("")),
      "Reason: Cannot set empty script"
    )
    notMiner.assertBalances(firstAddress, balance, eff)
  }

  test("non-issuer cannot change script") {
    /*
    issuer is first address, but script allows make SetAssetScript only second address
     */
    val assetWAnotherOwner = sender
      .issue(
        firstAddress,
        "NonOwnCoin",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        script = Some(
          ScriptCompiler(
            s"""
                                        |match tx {
                                        |case s : SetAssetScriptTransaction => s.sender == addressFromPublicKey(base58'${ByteStr(
                 pkByAddress(secondAddress).publicKey).base58}')
                                        |case _ => false}""".stripMargin,
            isAssetScript = true
          ).explicitGet()._1.bytes.value.base64)
      )
      .id
    nodes.waitForHeightAriseAndTxPresent(assetWAnotherOwner)

    assertBadRequestAndMessage(sender.setAssetScript(assetWAnotherOwner, secondAddress, setAssetScriptFee, Some(scriptBase64)),
                               "Reason: Asset was issued by other address")
    assertBadRequestAndMessage(sender.setAssetScript(assetWAnotherOwner, secondAddress, setAssetScriptFee, Some("")),
                               "Reason: Cannot set empty script")
  }

  test("non-issuer cannot change script on asset w/o script") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    assertBadRequestAndMessage(sender.setAssetScript(assetWOScript, secondAddress, setAssetScriptFee, Some(scriptBase64)),
                               "Reason: Asset was issued by other address")
    assertBadRequestAndMessage(sender.setAssetScript(assetWOScript, secondAddress, setAssetScriptFee), "Reason: Cannot set empty script")
    assertBadRequestAndMessage(
      sender.setAssetScript(assetWOScript, secondAddress, setAssetScriptFee, Some("")),
      "Reason: Cannot set empty script"
    )

    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("sender's waves balance is decreased by fee") {
    val script2 = ScriptCompiler(
      s"""
           |match tx {
           |  case s : SetAssetScriptTransaction => true
           |  case _ => false
           |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64

    val (balance, eff) = notMiner.accountBalances(firstAddress)
    val details        = notMiner.assetsDetails(assetWScript, true).scriptDetails.getOrElse(fail("Expecting to get asset details"))
    assert(details.scriptComplexity == 1)
    assert(details.scriptText == "TRUE") // [WAIT] true
    assert(details.script == scriptBase64)

    val txId = sender.setAssetScript(assetWScript, firstAddress, setAssetScriptFee, Some(script2)).id
    nodes.waitForHeightAriseAndTxPresent(txId)
    notMiner.assertBalances(firstAddress, balance - setAssetScriptFee, eff - setAssetScriptFee)
    val details2 = notMiner.assetsDetails(assetWScript, true).scriptDetails.getOrElse(fail("Expecting to get asset details"))
    assert(details2.scriptComplexity == 18)
    assert(details2.script == script2)
  }

  test("cannot transact without having enough waves") {
    val (balance, eff) = notMiner.accountBalances(firstAddress)
    assertBadRequestAndResponse(sender.setAssetScript(assetWScript, firstAddress, balance + 1, Some(scriptBase64)), "negative waves balance")
    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance, eff)

    val leaseAmount = 1.waves
    val leaseId     = sender.lease(firstAddress, secondAddress, leaseAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseId)

    assertBadRequestAndResponse(sender.setAssetScript(assetWScript, firstAddress, balance - leaseAmount, Some(scriptBase64)),
                                "negative effective balance")
    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance - minFee, eff - leaseAmount - minFee)
  }

  test("invalid transaction should not be in UTX or blockchain") {
    def sastx(fee: Long = setAssetScriptFee,
              timestamp: Long = System.currentTimeMillis,
              assetId: IssuedAsset = IssuedAsset(ByteStr.decodeBase58(assetWScript).get)): SetAssetScriptTransaction =
      SetAssetScriptTransaction
        .signed(AddressScheme.current.chainId, sender.privateKey, assetId, Some(script), fee, timestamp, sender.privateKey)
        .right
        .get

    val (balance, eff) = notMiner.accountBalances(firstAddress)

    val invalidTxs = Seq(
      (sastx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
      (sastx(fee = 9999999), "Fee .* does not exceed minimal value"),
      (sastx(assetId = IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz9ekQuYn92natMnMq8").get)), "invalid.assetId"),
      (sastx(assetId = IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get)), "Referenced assetId not found")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(tx.json()), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance, eff)
  }

  test("transaction requires a valid proof") {
    def request: JsObject = {
      val rs = sender.postJsonWithApiKey(
        "/transactions/sign",
        Json.obj(
          "version" -> 1,
          "type"    -> SetAssetScriptTransaction.typeId,
          "sender"  -> firstAddress,
          "fee"     -> setAssetScriptFee,
          "assetId" -> assetWScript,
          "script"  -> Some(scriptBase64)
        )
      )
      Json.parse(rs.getResponseBody).as[JsObject]
    }
    def id(obj: JsObject) = obj.value("id").as[String]

    val noProof = request - "proofs"
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), "failed to parse json message.*proofs.*missing")
    nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

    val badProof = request ++ Json.obj("proofs" -> Seq(Base58.encode(Array.fill(64)(Random.nextInt.toByte))))
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "proof doesn't validate as signature")
    nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

    val withProof = request
    assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
    sender.postJson("/transactions/broadcast", withProof)
    nodes.waitForHeightAriseAndTxPresent(id(withProof))
  }

  test("try to update script to null") {
    assertBadRequestAndResponse(sender.setAssetScript(assetWScript, firstAddress, setAssetScriptFee), "Reason: Cannot set empty script")
    assertBadRequestAndResponse(sender.setAssetScript(assetWScript, firstAddress, setAssetScriptFee, Some("")), "Reason: Cannot set empty script")
  }

  test("try to make SetAssetScript tx on script that deprecates SetAssetScript") {
    /*
    script doesn't allow do SetAssetScript
     */
    val assetUnchangeableScript = sender
      .issue(
        firstAddress,
        "SetAssetWDep",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        script = Some(unchangeableScript.bytes.value.base64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(assetUnchangeableScript)

    assertBadRequestAndResponse(sender.setAssetScript(assetUnchangeableScript, firstAddress, setAssetScriptFee, Some(scriptBase64)),
                                errNotAllowedByToken)
  }

  test("non-issuer can change script if issuer's account script allows (proof correct)") {
    val accountA = pkByAddress(firstAddress)

    val setScriptTransaction = SetScriptTransaction
      .selfSigned(
        accountA,
        Some(
          ScriptCompiler(
            s"""|let pkB = base58'${ByteStr(accountB.publicKey)}'
                                |match tx {
                                |case s : SetAssetScriptTransaction => sigVerify(s.bodyBytes,s.proofs[0],pkB)
                                |case _ => true}""".stripMargin,
            isAssetScript = false
          ).explicitGet()._1),
        setScriptFee,
        System.currentTimeMillis()
      )
      .right
      .get

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json())
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val nonIssuerUnsignedTx = SetAssetScriptTransaction(
      AddressScheme.current.chainId,
      accountA,
      IssuedAsset(ByteStr.decodeBase58(assetWScript).get),
      Some(unchangeableScript),
      setAssetScriptFee + 0.004.waves,
      System.currentTimeMillis,
      Proofs.empty
    )

    val sigTxB = ByteStr(crypto.sign(accountB, nonIssuerUnsignedTx.bodyBytes()))

    val signedTxByB =
      nonIssuerUnsignedTx.copy(proofs = Proofs(Seq(sigTxB)))

    val tx =
      sender.signedBroadcast(signedTxByB.json()).id

    nodes.waitForHeightAriseAndTxPresent(tx)

    //try to change unchangeable script
    val nonIssuerUnsignedTx2 = SetAssetScriptTransaction(
      AddressScheme.current.chainId,
      accountA,
      IssuedAsset(ByteStr.decodeBase58(assetWScript).get),
      Some(script),
      setAssetScriptFee + 0.004.waves,
      System.currentTimeMillis,
      Proofs.empty
    )

    val sigTxB2 = ByteStr(crypto.sign(accountB, nonIssuerUnsignedTx2.bodyBytes()))

    val signedTxByB2 =
      nonIssuerUnsignedTx2.copy(proofs = Proofs(Seq(sigTxB2)))

    assertBadRequestAndMessage(
      sender.signedBroadcast(signedTxByB2.json()),
      errNotAllowedByToken
    )
  }

  test("try to make SetAssetScript for asset v1") {
    val assetV1 = sender
      .issue(thirdAddress, "AssetV1", "Test coin for V1", someAssetAmount, 0, reissuable = false, issueFee)
      .id
    nodes.waitForHeightAriseAndTxPresent(assetV1)

    val (balance1, eff1) = notMiner.accountBalances(thirdAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    assertBadRequestAndMessage(sender.setAssetScript(assetV1, thirdAddress, setAssetScriptFee, Some(scriptBase64)).id,
                               "Reason: Cannot set script on an asset issued without a script")
    assertBadRequestAndMessage(sender.setAssetScript(assetV1, secondAddress, setAssetScriptFee, Some(scriptBase64)),
                               "Reason: Asset was issued by other address")
    notMiner.assertBalances(thirdAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)

  }

}
