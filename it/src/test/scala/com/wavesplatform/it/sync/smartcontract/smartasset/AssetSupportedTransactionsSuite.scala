package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.{ByteStr, IntegerDataEntry}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.BurnTransactionV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import play.api.libs.json.JsNumber

import scala.concurrent.duration._

class AssetSupportedTransactionsSuite extends BaseTransactionSuite {
  var asset = ""

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
        Some(scriptBase64),
        waitForTx = true
      )
      .id
  }

  test("transfer verification with asset script") {
    val firstAssetBalance  = sender.assetBalance(firstAddress, asset).balance
    val secondAssetBalance = sender.assetBalance(secondAddress, asset).balance
    val thirdAssetBalance  = sender.assetBalance(thirdAddress, asset).balance

    sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    notMiner.assertAssetBalance(firstAddress, asset, firstAssetBalance - 100)
    notMiner.assertAssetBalance(secondAddress, asset, secondAssetBalance + 100)

    sender.transfer(secondAddress, thirdAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    //deprecate transfers with amount > 99
    val scr = ScriptCompiler(
      s"""
                                    |match tx {
                                    |  case s : SetAssetScriptTransaction => true
                                    |  case t:  TransferTransaction => t.amount <= 99
                                    |  case _ => false
                                    |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertBadRequestAndMessage(sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(asset)), errNotAllowedByToken)

    assertBadRequestAndMessage(sender.transfer(thirdAddress, secondAddress, 100, smartMinFee, Some(asset)), errNotAllowedByToken)

    sender.transfer(thirdAddress, secondAddress, 99, smartMinFee, Some(asset), waitForTx = true)

    notMiner.assertAssetBalance(secondAddress, asset, secondAssetBalance + 99)
    notMiner.assertAssetBalance(thirdAddress, asset, thirdAssetBalance + 1)
  }

  test("transfer goes only to addresses from list (white or black)") {
    val scr = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case t:  TransferTransaction => t.recipient == addressFromPublicKey(base58'${ByteStr(
           pkByAddress(secondAddress).publicKey).base58}')
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    assertBadRequestAndMessage(sender.transfer(firstAddress, thirdAddress, 100, smartMinFee, Some(asset)), errNotAllowedByToken)

    assertBadRequestAndMessage(sender.transfer(firstAddress, firstAddress, 1, smartMinFee, Some(asset)), errNotAllowedByToken)

    val scr1 = ScriptCompiler(
      s"""
                                |match tx {
                                |  case s : SetAssetScriptTransaction => true
                                |  case t:  TransferTransaction => t.recipient != addressFromPublicKey(base58'${ByteStr(
           pkByAddress(secondAddress).publicKey).base58}') && t.recipient != addressFromPublicKey(base58'${ByteStr(
           pkByAddress(firstAddress).publicKey).base58}')
                                |  case _ => false
                                |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr1), waitForTx = true)

    sender.transfer(firstAddress, thirdAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    assertBadRequestAndMessage(sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(asset)), errNotAllowedByToken)

    assertBadRequestAndMessage(sender.transfer(firstAddress, firstAddress, 1, smartMinFee, Some(asset)), errNotAllowedByToken)
  }

  test("smart asset requires fee in other asset") {
    val feeAsset = sender
      .issue(firstAddress, "FeeAsset", "Asset for fee of Smart Asset", someAssetAmount, 2, reissuable = false, issueFee, waitForTx = true)
      .id

    sender.sponsorAsset(firstAddress, feeAsset, baseFee = 2, fee = sponsorFee + smartFee, waitForTx = true)

    val scr = ScriptCompiler(
      s"""
                                |match tx {
                                |  case s : SetAssetScriptTransaction => true
                                |  case t:  TransferTransaction => t.feeAssetId == base58'$feeAsset'
                                |  case _ => false
                                |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertBadRequestAndMessage(sender.transfer(firstAddress, thirdAddress, 100, 2, Some(asset), feeAssetId = Some(feeAsset)),
                               "does not exceed minimal value")

    sender.transfer(firstAddress, thirdAddress, 100, 10, Some(asset), feeAssetId = Some(feeAsset), waitForTx = true)

    assertBadRequestAndMessage(sender.transfer(firstAddress, firstAddress, 1, smartMinFee, Some(asset)), errNotAllowedByToken)

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
        Some(scriptBase64),
        waitForTx = true
      )
      .id

    sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(blackAsset), waitForTx = true)

    val scr = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case t:  TransferTransaction => let issuer = extract(addressFromString("${firstAddress}"))
                                        |  isDefined(getInteger(issuer,toBase58String(t.id))) == true
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(blackAsset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    val blackTx = TransferTransactionV2
      .selfSigned(
        2,
        Some(ByteStr.decodeBase58(blackAsset).get),
        pkByAddress(secondAddress),
        pkByAddress(thirdAddress),
        1,
        System.currentTimeMillis + 1.minutes.toMillis,
        None,
        smartMinFee,
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
        smartMinFee,
        Array.emptyByteArray
      )
      .right
      .get

    val dataTx = sender.putData(firstAddress, List(IntegerDataEntry(s"${blackTx.id.value.base58}", 42)), minFee).id
    nodes.waitForHeightAriseAndTxPresent(dataTx)

    sender.signedBroadcast(blackTx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)), waitForTx = true)

    assertBadRequestAndMessage(sender.signedBroadcast(incorrectTx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))),
                               errNotAllowedByToken)
  }

  test("burner is from the list (white or black)") {
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scriptBase64), waitForTx = true)

    sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    sender.transfer(firstAddress, thirdAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    val scr = ScriptCompiler(
      s"""
                                |match tx {
                                |  case s : SetAssetScriptTransaction => true
                                |  case b:  BurnTransaction => b.sender == addressFromPublicKey(base58'${ByteStr(pkByAddress(secondAddress).publicKey).base58}')
                                |  case _ => false
                                |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    sender.burn(secondAddress, asset, 10, smartMinFee, waitForTx = true)

    assertBadRequestAndMessage(sender.burn(firstAddress, asset, 10, smartMinFee), errNotAllowedByToken)

    val scr1 = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case b:  BurnTransaction => b.sender != addressFromPublicKey(base58'${ByteStr(
           pkByAddress(secondAddress).publicKey).base58}')
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr1), waitForTx = true)

    sender.burn(thirdAddress, asset, 10, smartMinFee, waitForTx = true)

    sender.burn(firstAddress, asset, 10, smartMinFee, waitForTx = true)

    assertBadRequestAndMessage(sender.burn(secondAddress, asset, 10, smartMinFee), errNotAllowedByToken)
  }

  ignore("burn by some height") {
    val scr = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case b:  BurnTransaction => height % 2 == 0
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    if (nodes.map(_.height).max % 2 != 0) nodes.waitForHeightArise()

    sender.burn(firstAddress, asset, 10, smartMinFee, waitForTx = true)

    if (nodes.map(_.height).max % 2 == 0) {
      nodes.waitForHeightArise()
    }

    assertBadRequestAndMessage(sender.burn(firstAddress, asset, 10, smartMinFee), errNotAllowedByToken)
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
        Some(
          ScriptCompiler(
            s"""
                               |match tx {
                               |  case b : BurnTransaction => false
                               |  case _ => true
                               |}
         """.stripMargin,
            isAssetScript = true
          ).explicitGet()._1.bytes.value.base64),
        waitForTx = true
      )
      .id

    assertBadRequestAndMessage(sender.burn(firstAddress, unBurnable, 10, smartMinFee).id, errNotAllowedByToken)
  }

  test("masstransfer - taxation") {
    val scr = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case m:  MassTransferTransaction => 
                                        |  let twoTransfers = size(m.transfers) == 2
                                        |  let issuerIsRecipient = m.transfers[0].recipient == addressFromString("${firstAddress}")
                                        |  let taxesPaid = m.transfers[0].amount >= m.transfers[1].amount / 10
                                        |  twoTransfers && issuerIsRecipient && taxesPaid
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    val transfers       = List(Transfer(firstAddress, 10), Transfer(secondAddress, 100))
    val massTransferFee = calcMassTransferFee(transfers.size)
    sender.massTransfer(firstAddress, transfers, massTransferFee + smartFee, Some(asset), waitForTx = true)

    val transfers2 = List(Transfer(firstAddress, 9), Transfer(secondAddress, 100))
    assertBadRequestAndMessage(sender.massTransfer(firstAddress, transfers2, massTransferFee + smartFee, Some(asset)), errNotAllowedByToken)
  }

  test("masstransfer - transferCount <=2") {
    val scr = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case m:  MassTransferTransaction => 
                                        |  m.transferCount <= 2
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    val transfers                  = List(Transfer(firstAddress, 10), Transfer(secondAddress, 100), Transfer(firstAddress, 10))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    assertBadRequestAndMessage(sender.massTransfer(firstAddress, transfers, massTransferTransactionFee + smartFee, Some(asset)), errNotAllowedByToken)
  }

  test("reissue by non-issuer") {
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scriptBase64), waitForTx = true)

    assertBadRequestAndMessage(sender.reissue(secondAddress, asset, someAssetAmount, reissuable = true, fee = issueFee + smartFee),
                               "Reason: Asset was issued by other address")

    val scr = ScriptCompiler(
      s"""
                             |match tx {
                             |  case s : SetAssetScriptTransaction => true
                             |  case r:  ReissueTransaction => r.sender == addressFromPublicKey(base58'${ByteStr(pkByAddress(secondAddress).publicKey).base58}')
                             |  case _ => false
                             |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertBadRequestAndMessage(sender.reissue(secondAddress, asset, someAssetAmount, reissuable = true, fee = issueFee + smartFee),
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
        Some(scriptBase64),
        waitForTx = true
      )
      .id

    val scr = ScriptCompiler(
      s"""
                             |match tx {
                             |  case s : SetAssetScriptTransaction => true
                             |  case r:  ReissueTransaction => r.sender == addressFromPublicKey(base58'${ByteStr(pkByAddress(secondAddress).publicKey).base58}')
                             |  case _ => false
                             |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(assetNonReissue, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertBadRequestAndMessage(sender.reissue(secondAddress, assetNonReissue, someAssetAmount, reissuable = true, fee = issueFee + smartFee),
                               "Asset is not reissuable")

    assertBadRequestAndMessage(sender.reissue(firstAddress, assetNonReissue, someAssetAmount, reissuable = true, fee = issueFee + smartFee),
                               "Asset is not reissuable")
  }

  test("sponsorship of smart asset") {
    assertBadRequestAndMessage(sender.sponsorAsset(firstAddress, asset, baseFee = 2, fee = sponsorFee + smartFee), errNotAllowedByToken)

    val scr = ScriptCompiler(
      s"""
                                        |match tx {
                                        |  case s : SetAssetScriptTransaction => true
                                        |  case ss: SponsorFeeTransaction => ss.sender == addressFromPublicKey(base58'${ByteStr(
           pkByAddress(firstAddress).publicKey).base58}')
                                        |  case _ => false
                                        |}
         """.stripMargin,
      isAssetScript = true
    ).explicitGet()._1.bytes.value.base64
    sender.setAssetScript(asset, firstAddress, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertBadRequestAndMessage(sender.sponsorAsset(firstAddress, asset, baseFee = 2, fee = sponsorFee + smartFee),
                               "Reason: Sponsorship smart assets is disabled.")

  }

  test("try to send transactions forbidden by the asset's script") {
    val assetWOSupport = sender
      .issue(
        firstAddress,
        "assetWOSuppor",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        script = Some(ScriptCompiler(s"false".stripMargin, isAssetScript = true).explicitGet()._1.bytes.value.base64),
        waitForTx = true
      )
      .id

    assertBadRequestAndMessage(sender.setAssetScript(assetWOSupport, firstAddress, smartMinFee, Some(scriptBase64)), errNotAllowedByToken)
    assertBadRequestAndMessage(sender.transfer(firstAddress, secondAddress, 100, smartMinFee, Some(assetWOSupport)), errNotAllowedByToken)
    assertBadRequestAndMessage(sender.burn(firstAddress, assetWOSupport, 10, smartMinFee), errNotAllowedByToken)
    assertBadRequestAndMessage(sender.reissue(firstAddress, assetWOSupport, someAssetAmount, true, issueFee + smartFee), "Asset is not reissuable")

    val transfers = List(Transfer(firstAddress, 10))
    assertBadRequestAndMessage(sender.massTransfer(firstAddress, transfers, calcMassTransferFee(transfers.size) + smartFee, Some(assetWOSupport)),
                               errNotAllowedByToken)

  }

  test("try to use proofs in assets script") {
    val errProofMsg = "Reason: Script doesn't exist and proof doesn't validate as signature"
    val assetWProofs = sender
      .issue(
        firstAddress,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        script = Some(
          ScriptCompiler(
            s"""
                                 let proof = base58'assetWProofs'
                                 match tx {
                                   case tx: SetAssetScriptTransaction | TransferTransaction | ReissueTransaction | BurnTransaction => tx.proofs[0] == proof
                                   case _ => false
                                 }""".stripMargin,
            false
          ).explicitGet()._1.bytes.value.base64),
        waitForTx = true
      )
      .id

    val incorrectTrTx = TransferTransactionV2
      .create(
        2,
        Some(ByteStr.decodeBase58(assetWProofs).get),
        pkByAddress(firstAddress),
        pkByAddress(thirdAddress),
        1,
        System.currentTimeMillis + 10.minutes.toMillis,
        None,
        smartMinFee,
        Array.emptyByteArray,
        Proofs(Seq(ByteStr("assetWProofs".getBytes())))
      )
      .right
      .get

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectTrTx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))),
      errProofMsg
    )

    val incorrectBrTx = BurnTransactionV2
      .create(
        2,
        AddressScheme.current.chainId,
        pkByAddress(firstAddress),
        ByteStr.decodeBase58(assetWProofs).get,
        1,
        smartMinFee,
        System.currentTimeMillis + 10.minutes.toMillis,
        Proofs(Seq(ByteStr("assetWProofs".getBytes())))
      )
      .right
      .get

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectBrTx.json() + ("type" -> JsNumber(BurnTransactionV2.typeId.toInt))),
      errProofMsg
    )
  }
}
