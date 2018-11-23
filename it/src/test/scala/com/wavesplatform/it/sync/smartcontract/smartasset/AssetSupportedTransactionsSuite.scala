package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.{ByteStr, IntegerDataEntry}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import play.api.libs.json.JsNumber
import com.wavesplatform.state._
import scala.concurrent.duration._

class AssetSupportedTransactionsSuite extends BaseTransactionSuite {
  var asset                = ""
  private val verySmartFee = minFee + smartFee

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    asset = sender
      .issue(
        firstAddress,
        "MyAsset",
        "Test Asset",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        Some(scriptBase64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(asset)
  }

  test("transfer verification with asset script") {
    val firstAssetBalance  = sender.assetBalance(firstAddress, asset).balance
    val secondAssetBalance = sender.assetBalance(secondAddress, asset).balance
    val thirdAssetBalance  = sender.assetBalance(thirdAddress, asset).balance

    val tTxId = sender.transfer(firstAddress, secondAddress, 100, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(tTxId)

    notMiner.assertAssetBalance(firstAddress, asset, firstAssetBalance - 100)
    notMiner.assertAssetBalance(secondAddress, asset, secondAssetBalance + 100)

    val tTxId1 = sender.transfer(secondAddress, thirdAddress, 100, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(tTxId1)

    //deprecate transfers with amount <= 99
    val scr         = ScriptCompiler(s"""
                                    |match tx {
                                    |  case s : SetAssetScriptTransaction => true
                                    |  case t:  TransferTransaction => t.amount <= 99
                                    |  case _ => false
                                    |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    assertBadRequestAndMessage(sender.transfer(firstAddress, secondAddress, 100, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")

    assertBadRequestAndMessage(sender.transfer(thirdAddress, secondAddress, 100, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")

    val tTxId2 = sender.transfer(thirdAddress, secondAddress, 99, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(tTxId2)

    notMiner.assertAssetBalance(secondAddress, asset, secondAssetBalance + 99)
    notMiner.assertAssetBalance(thirdAddress, asset, thirdAssetBalance + 1)
  }

  test("transfer goes only to addresses from list (white or black)") {
    val scr = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case t:  TransferTransaction => t.recipient == addressFromPublicKey(base58'${ByteStr(
                                  pkByAddress(secondAddress).publicKey).base58}')
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val tTxId = sender.transfer(firstAddress, secondAddress, 100, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(tTxId)

    assertBadRequestAndMessage(sender.transfer(firstAddress, thirdAddress, 100, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")

    assertBadRequestAndMessage(sender.transfer(firstAddress, firstAddress, 1, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")

    val scr1 = ScriptCompiler(s"""
                                |match tx {
                                |  case s : SetAssetScriptTransaction => true
                                |  case t:  TransferTransaction => t.recipient != addressFromPublicKey(base58'${ByteStr(
                                   pkByAddress(secondAddress).publicKey).base58}') && t.recipient != addressFromPublicKey(base58'${ByteStr(
                                   pkByAddress(firstAddress).publicKey).base58}')
                                |  case _ => false
                                |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId1 = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr1)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId1)

    val tTxId1 = sender.transfer(firstAddress, thirdAddress, 100, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(tTxId1)

    assertBadRequestAndMessage(sender.transfer(firstAddress, secondAddress, 100, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")

    assertBadRequestAndMessage(sender.transfer(firstAddress, firstAddress, 1, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")
  }

  test("smart asset requires fee in other asset") {
    val feeAsset = sender
      .issue(firstAddress, "FeeAsset", "Asset for fee of Smart Asset", someAssetAmount, 2, reissuable = false, issueFee)
      .id
    nodes.waitForHeightAriseAndTxPresent(feeAsset)

    val sponsorId = sender.sponsorAsset(firstAddress, feeAsset, baseFee = 2, fee = sponsorFee + smartFee).id
    nodes.waitForHeightAriseAndTxPresent(sponsorId)

    val scr         = ScriptCompiler(s"""
                                |match tx {
                                |  case s : SetAssetScriptTransaction => true
                                |  case t:  TransferTransaction => t.feeAssetId == base58'$feeAsset'
                                |  case _ => false
                                |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    assertBadRequestAndMessage(sender.transfer(firstAddress, thirdAddress, 100, 2, Some(asset), feeAssetId = Some(feeAsset)),
                               "does not exceed minimal value")

    val txId = sender.transfer(firstAddress, thirdAddress, 100, 10, Some(asset), feeAssetId = Some(feeAsset)).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    assertBadRequestAndMessage(sender.transfer(firstAddress, firstAddress, 1, verySmartFee, Some(asset)),
                               "Transaction is not allowed by token-script")

  }

  test("token that can be only transferred with the issuer's permission - black label") {
    val blackAsset = sender
      .issue(
        firstAddress,
        "BlackAsset",
        "Test Asset",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        Some(scriptBase64)
      )
      .id
    nodes.waitForHeightAriseAndTxPresent(blackAsset)

    val tTxId = sender.transfer(firstAddress, secondAddress, 100, verySmartFee, Some(blackAsset)).id
    nodes.waitForHeightAriseAndTxPresent(tTxId)

    val scr         = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case t:  TransferTransaction => let issuer = extract(addressFromString("${firstAddress}"))
                                        |  isDefined(getInteger(issuer,toBase58String(t.id))) == true
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(blackAsset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val blackTx = TransferTransactionV2
      .selfSigned(
        2,
        Some(ByteStr.decodeBase58(blackAsset).get),
        pkByAddress(secondAddress),
        pkByAddress(thirdAddress),
        1,
        System.currentTimeMillis + 1.minutes.toMillis,
        None,
        verySmartFee,
        Array.emptyByteArray
      )
      .right
      .get

    val incorrectTx = TransferTransactionV2
      .selfSigned(
        2,
        Some(ByteStr.decodeBase58(blackAsset).get),
        pkByAddress(secondAddress),
        pkByAddress(thirdAddress),
        1,
        System.currentTimeMillis + 10.minutes.toMillis,
        None,
        verySmartFee,
        Array.emptyByteArray
      )
      .right
      .get

    val dataTx = sender.putData(firstAddress, List(IntegerDataEntry(s"${blackTx.id.value.base58}", 42)), minFee).id
    nodes.waitForHeightAriseAndTxPresent(dataTx)

    val txId = sender.signedBroadcast(blackTx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    assertBadRequestAndMessage(sender.signedBroadcast(incorrectTx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id,
                               "Transaction is not allowed by token-script")
  }

  test("burner is from the list (white or black)") {
    val setTrueId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scriptBase64)).id
    nodes.waitForHeightAriseAndTxPresent(setTrueId)

    val toSecond = sender.transfer(firstAddress, secondAddress, 100, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(toSecond)

    val toThird = sender.transfer(firstAddress, thirdAddress, 100, verySmartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(toThird)

    val scr         = ScriptCompiler(s"""
                                |match tx {
                                |  case s : SetAssetScriptTransaction => true
                                |  case b:  BurnTransaction => b.sender == addressFromPublicKey(base58'${ByteStr(pkByAddress(secondAddress).publicKey).base58}')
                                |  case _ => false
                                |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val burnNonissuerTx = sender.burn(secondAddress, asset, 10, verySmartFee).id
    nodes.waitForHeightAriseAndTxPresent(burnNonissuerTx)

    assertBadRequestAndMessage(sender.burn(firstAddress, asset, 10, verySmartFee).id, "Transaction is not allowed by token-script")

    val scr1 = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case b:  BurnTransaction => b.sender != addressFromPublicKey(base58'${ByteStr(
                                   pkByAddress(secondAddress).publicKey).base58}')
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId1 = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr1)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId1)

    val burnNonissuerTx1 = sender.burn(thirdAddress, asset, 10, verySmartFee).id
    nodes.waitForHeightAriseAndTxPresent(burnNonissuerTx1)

    val burnIssuerTx2 = sender.burn(firstAddress, asset, 10, verySmartFee).id
    nodes.waitForHeightAriseAndTxPresent(burnIssuerTx2)

    assertBadRequestAndMessage(sender.burn(secondAddress, asset, 10, verySmartFee).id, "Transaction is not allowed by token-script")
  }

  test("burn by some height") {
    val scr         = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case b:  BurnTransaction => height % 2 == 0
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    if (nodes.map(_.height).max % 2 != 0) nodes.waitForHeightArise()

    val burnIssuerTx = sender.burn(firstAddress, asset, 10, verySmartFee).id
    nodes.waitForHeightAriseAndTxPresent(burnIssuerTx)

    if (nodes.map(_.height).max % 2 == 0) {
      nodes.waitForHeightArise()
    }

    assertBadRequestAndMessage(sender.burn(firstAddress, asset, 10, verySmartFee).id, "Transaction is not allowed by token-script")
  }

  test("unburable asset") {
    val unBurnable = sender
      .issue(
        firstAddress,
        "Unburnable",
        "Test Asset",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        Some(ScriptCompiler(s"""
                               |match tx {
                               |  case b : BurnTransaction => false
                               |  case _ => true
                               |}
         """.stripMargin).explicitGet()._1.bytes.value.base64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(unBurnable)

    assertBadRequestAndMessage(sender.burn(firstAddress, unBurnable, 10, verySmartFee).id, "Transaction is not allowed by token-script")
  }

  test("masstransfer - taxation") {
    val scr         = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case m:  MassTransferTransaction => 
                                        |  let twoTransfers = size(m.transfers) == 2
                                        |  let issuerIsRecipient = m.transfers[0].recipient == addressFromString("${firstAddress}")
                                        |  let taxesPaid = m.transfers[0].amount >= m.transfers[1].amount / 10
                                        |  twoTransfers && issuerIsRecipient && taxesPaid
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val transfers                  = List(Transfer(firstAddress, 10), Transfer(secondAddress, 100))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    val transferId                 = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee + smartFee, Some(asset)).id
    nodes.waitForHeightAriseAndTxPresent(transferId)

  }

  test("masstransfer - transferCount <=2") {
    val scr         = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case m:  MassTransferTransaction => 
                                        |  m.transferCount <= 2
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val transfers                  = List(Transfer(firstAddress, 10), Transfer(secondAddress, 100), Transfer(firstAddress, 10))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    assertBadRequestAndMessage(sender.massTransfer(firstAddress, transfers, massTransferTransactionFee + smartFee, Some(asset)).id,
                               "Transaction is not allowed by token-script")
  }

  test("reissue by issuer and non-issuer") {
    val setTrueId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scriptBase64)).id
    nodes.waitForHeightAriseAndTxPresent(setTrueId)

    assertBadRequestAndMessage(sender.reissue(secondAddress, asset, someAssetAmount, reissuable = true, fee = issueFee + smartFee).id,
                               "Reason: Asset was issued by other address")

    val scr         = ScriptCompiler(s"""
                             |match tx {
                             |  case s : SetAssetScriptTransaction => true
                             |  case r:  ReissueTransaction => r.sender == addressFromPublicKey(base58'${ByteStr(pkByAddress(secondAddress).publicKey).base58}')
                             |  case _ => false
                             |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    assertBadRequestAndMessage(sender.reissue(secondAddress, asset, someAssetAmount, reissuable = true, fee = issueFee + smartFee).id,
                               "Reason: Asset was issued by other address")
  }

  test("reissue by issuer and non-issuer non-re issuable smart asset ") {
    val assetNonReissue = sender
      .issue(
        firstAddress,
        "MyAsset",
        "Test Asset",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        Some(scriptBase64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(assetNonReissue)

    val scr         = ScriptCompiler(s"""
                             |match tx {
                             |  case s : SetAssetScriptTransaction => true
                             |  case r:  ReissueTransaction => r.sender == addressFromPublicKey(base58'${ByteStr(pkByAddress(secondAddress).publicKey).base58}')
                             |  case _ => false
                             |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(assetNonReissue, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    assertBadRequestAndMessage(sender.reissue(secondAddress, assetNonReissue, someAssetAmount, reissuable = true, fee = issueFee + smartFee).id,
                               "Asset is not reissuable")

    assertBadRequestAndMessage(sender.reissue(firstAddress, assetNonReissue, someAssetAmount, reissuable = true, fee = issueFee + smartFee).id,
                               "Asset is not reissuable")
  }

  test("sponsorship of smart asset") {
    assertBadRequestAndMessage(sender.sponsorAsset(firstAddress, asset, baseFee = 2, fee = sponsorFee + smartFee).id,
                               "Transaction is not allowed by token-script")

    val scr = ScriptCompiler(s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case ss: SponsorFeeTransaction => ss.sender == addressFromPublicKey(base58'${ByteStr(
                                  pkByAddress(firstAddress).publicKey).base58}')
                                        |  case _ => false
                                        |}
         """.stripMargin).explicitGet()._1.bytes.value.base64
    val setScriptId = sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr)).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    assertBadRequestAndMessage(sender.sponsorAsset(firstAddress, asset, baseFee = 2, fee = sponsorFee + smartFee).id,
                               "Reason: Sponsorship smart assets is disabled.")

  }

}
