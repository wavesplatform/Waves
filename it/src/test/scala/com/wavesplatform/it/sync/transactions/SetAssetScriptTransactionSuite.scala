package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.assets.SignedSetAssetScriptRequest
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.someAssetAmount
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.smart.script.{ScriptCompiler}
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import scorex.crypto.encode.Base58
import play.api.libs.json._
import scala.util.Random
import scala.concurrent.duration._

class SetAssetScriptTransactionSuite extends BaseTransactionSuite {
  var testAssetWOScript     = ""
  var testAssetWScript      = ""
  var testAssetWDeprecation = ""
  val setAssetScriptFee     = 1.waves + 0.004.waves
  val scriptWithDeprecation = ScriptCompiler(s"""
                                               |match tx {
                                               |case s : SetAssetScriptTransaction => false
                                               |case _ => true}""".stripMargin).explicitGet()._1

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    testAssetWOScript = sender
      .issue(firstAddress, "AssetWOScript", "Test coin for SetAssetScript tests w/o script", someAssetAmount, 0, reissuable = false, issueFee, 2)
      .id

    testAssetWScript = sender
      .issue(firstAddress,
             "SetAssetScript",
             "Test coin for SetAssetScript tests",
             someAssetAmount,
             0,
             reissuable = false,
             issueFee,
             2,
             script = Some(scriptBase64))
      .id

    testAssetWDeprecation = sender
      .issue(
        firstAddress,
        "SetAssetWDep",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        script = Some(scriptWithDeprecation.bytes.value.base64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(testAssetWScript)
    nodes.waitForHeightAriseAndTxPresent(testAssetWDeprecation)
  }

  test("cannot set script on asset w/o initial script") {
    val (balance, eff) = notMiner.accountBalances(firstAddress)
    assertBadRequestAndMessage(sender.setAssetScript(testAssetWOScript, firstAddress, setAssetScriptFee, Some(scriptBase64)), "")
    notMiner.assertBalances(firstAddress, balance, eff)
  }

  test("sender's waves balance is decreased by fee") {
    val script = ScriptCompiler(s"""
           |match tx {
           |  case s : SetScriptTransaction => true
           |  case _ => false
           |}
         """.stripMargin).explicitGet()._1.bytes.value.base64

    val (balance, eff) = notMiner.accountBalances(firstAddress)
    val txId           = sender.setAssetScript(testAssetWScript, firstAddress, setAssetScriptFee, Some(script)).id
    nodes.waitForHeightAriseAndTxPresent(txId)
    notMiner.assertBalances(firstAddress, balance - setAssetScriptFee, eff - setAssetScriptFee)
  }

  test("cannot transact without having enough waves") {
    val (balance, eff) = notMiner.accountBalances(firstAddress)
    assertBadRequestAndResponse(sender.setAssetScript(testAssetWScript, firstAddress, balance + 1, Some(scriptBase64)), "negative waves balance")
    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance, eff)

    val leaseAmount = 1.waves
    val leaseId     = sender.lease(firstAddress, secondAddress, leaseAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseId)

    assertBadRequestAndResponse(sender.setAssetScript(testAssetWScript, firstAddress, balance - leaseAmount, Some(scriptBase64)),
                                "negative effective balance")
    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance - minFee, eff - leaseAmount - minFee)
  }

  test("invalid transaction should not be in UTX or blockchain") {
    def sastx(version: Byte = SetAssetScriptTransaction.supportedVersions.head,
              fee: Long = setAssetScriptFee,
              timestamp: Long = System.currentTimeMillis,
              assetId: ByteStr = ByteStr.decodeBase58(testAssetWScript).get,
    ): SetAssetScriptTransaction =
      SetAssetScriptTransaction
        .signed(version, AddressScheme.current.chainId, sender.privateKey, assetId, Some(script), fee, timestamp, sender.privateKey)
        .right
        .get

    def request(tx: SetAssetScriptTransaction): SignedSetAssetScriptRequest =
      SignedSetAssetScriptRequest(
        SetAssetScriptTransaction.supportedVersions.head,
        Base58.encode(tx.sender.publicKey),
        tx.assetId.base58,
        Some(tx.script.get.bytes.value.base64),
        tx.fee,
        tx.timestamp,
        tx.proofs.base58().toList
      )

    implicit val w =
      Json.writes[SignedSetAssetScriptRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(SetAssetScriptTransaction.typeId.toInt)))

    val (balance, eff) = notMiner.accountBalances(firstAddress)
    val invalidTxs = Seq(
      (sastx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction .* is from far future"),
      (sastx(fee = 9999999), "Fee .* does not exceed minimal value"),
      (sastx(assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz9ekQuYn92natMnMq8").get), "invalid.assetId"),
      (sastx(assetId = ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get), "Referenced assetId not found")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(request(tx)), diag)
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
          "assetId" -> testAssetWScript,
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
    assertBadRequestAndResponse(sender.setAssetScript(testAssetWScript, firstAddress, setAssetScriptFee), "Reason: Empty script is disabled.")
  }

  test("try to make setassetscript tx on script what deprecated setassetscript") {
    assertBadRequestAndResponse(sender.setAssetScript(testAssetWDeprecation, firstAddress, setAssetScriptFee, Some(scriptBase64)),
                                "Transaction is not allowed by token-script")
  }

}
