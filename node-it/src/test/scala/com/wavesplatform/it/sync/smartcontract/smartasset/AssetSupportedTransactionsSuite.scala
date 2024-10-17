package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.TransferTransaction

import scala.concurrent.duration.*

class AssetSupportedTransactionsSuite extends BaseTransactionSuite {
  var asset = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    asset = sender
      .issue(
        firstKeyPair,
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

    sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    miner.assertAssetBalance(firstAddress, asset, firstAssetBalance - 100)
    miner.assertAssetBalance(secondAddress, asset, secondAssetBalance + 100)

    sender.transfer(secondKeyPair, thirdAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    // deprecate transfers with amount > 99
    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _ : SetAssetScriptTransaction => true
           |  case t:  TransferTransaction => t.amount <= 99
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertApiError(sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)

    assertApiError(sender.transfer(thirdKeyPair, secondAddress, 100, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)

    sender.transfer(thirdKeyPair, secondAddress, 99, smartMinFee, Some(asset), waitForTx = true)

    miner.assertAssetBalance(secondAddress, asset, secondAssetBalance + 99)
    miner.assertAssetBalance(thirdAddress, asset, thirdAssetBalance + 1)
  }

  test("transfer goes only to addresses from list (white or black)") {
    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case t:  TransferTransaction => t.recipient == addressFromPublicKey(base58'${secondKeyPair.publicKey}')
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    assertApiError(sender.transfer(firstKeyPair, thirdAddress, 100, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)

    assertApiError(sender.transfer(firstKeyPair, firstAddress, 1, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)

    val scr1 = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case t:  TransferTransaction => t.recipient != addressFromPublicKey(base58'${secondKeyPair.publicKey}') && t.recipient != addressFromPublicKey(base58'${firstKeyPair.publicKey}')
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr1), waitForTx = true)

    sender.transfer(firstKeyPair, thirdAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    assertApiError(sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)

    assertApiError(sender.transfer(firstKeyPair, firstAddress, 1, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)
  }

  test("smart asset requires fee in other asset") {
    val feeAsset = sender
      .issue(firstKeyPair, "FeeAsset", "Asset for fee of Smart Asset", someAssetAmount, 2, reissuable = false, issueFee, waitForTx = true)
      .id

    sender.sponsorAsset(firstKeyPair, feeAsset, baseFee = 2, fee = sponsorReducedFee + smartFee, waitForTx = true)

    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case t:  TransferTransaction => t.feeAssetId == base58'$feeAsset'
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertApiError(sender.transfer(firstKeyPair, thirdAddress, 100, 2, Some(asset), feeAssetId = Some(feeAsset))) { error =>
      error.message should include("does not exceed minimal value")
    }

    sender.transfer(firstKeyPair, thirdAddress, 100, 10, Some(asset), feeAssetId = Some(feeAsset), waitForTx = true)

    assertApiError(sender.transfer(firstKeyPair, firstAddress, 1, smartMinFee, Some(asset)), errNotAllowedByTokenApiError)

  }

  test("token that can be only transferred with the issuer's permission - black label") {
    val blackAsset = sender
      .issue(
        firstKeyPair,
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

    sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(blackAsset), waitForTx = true)

    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case t:  TransferTransaction => let issuer = extract(addressFromString("$firstAddress"))
           |  isDefined(getInteger(issuer,toBase58String(t.id))) == true
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(blackAsset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    val blackTx = TransferTransaction
      .selfSigned(
        2.toByte,
        secondKeyPair,
        thirdKeyPair.toAddress,
        IssuedAsset(ByteStr.decodeBase58(blackAsset).get),
        1,
        Waves,
        smartMinFee,
        ByteStr.empty,
        System.currentTimeMillis + 1.minutes.toMillis
      )
      .explicitGet()

    val incorrectTx = TransferTransaction
      .selfSigned(
        2.toByte,
        secondKeyPair,
        thirdKeyPair.toAddress,
        IssuedAsset(ByteStr.decodeBase58(blackAsset).get),
        1,
        Waves,
        smartMinFee,
        ByteStr.empty,
        System.currentTimeMillis + 10.minutes.toMillis
      )
      .explicitGet()

    val dataTx = sender.putData(firstKeyPair, List(IntegerDataEntry(s"${blackTx.id()}", 42)), minFee).id
    nodes.waitForHeightAriseAndTxPresent(dataTx)

    sender.signedBroadcast(blackTx.json(), waitForTx = true)

    assertApiError(sender.signedBroadcast(incorrectTx.json()), errNotAllowedByTokenApiError)
  }

  test("burner is from the list (white or black)") {
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scriptBase64), waitForTx = true)

    sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    sender.transfer(firstKeyPair, thirdAddress, 100, smartMinFee, Some(asset), waitForTx = true)

    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _ : SetAssetScriptTransaction => true
           |  case b:  BurnTransaction => b.sender == addressFromPublicKey(base58'${secondKeyPair.publicKey}')
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    sender.burn(secondKeyPair, asset, 10, smartMinFee, waitForTx = true)

    assertApiError(sender.burn(firstKeyPair, asset, 10, smartMinFee), errNotAllowedByTokenApiError)

    val scr1 = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case b:  BurnTransaction => b.sender != addressFromPublicKey(base58'${secondKeyPair.publicKey}')
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr1), waitForTx = true)

    sender.burn(thirdKeyPair, asset, 10, smartMinFee, waitForTx = true)

    sender.burn(firstKeyPair, asset, 10, smartMinFee, waitForTx = true)

    assertApiError(sender.burn(secondKeyPair, asset, 10, smartMinFee), errNotAllowedByTokenApiError)
  }

  ignore("burn by some height") {
    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case _: BurnTransaction => height % 2 == 0
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    if (nodes.map(_.height).max % 2 != 0) nodes.waitForHeightArise()

    sender.burn(firstKeyPair, asset, 10, smartMinFee, waitForTx = true)

    if (nodes.map(_.height).max % 2 == 0) {
      nodes.waitForHeightArise()
    }

    assertApiError(sender.burn(firstKeyPair, asset, 10, smartMinFee), errNotAllowedByTokenApiError)
  }

  test("unburnable asset") {
    val unBurnable = sender
      .issue(
        firstKeyPair,
        "Unburnable",
        "Test Asset",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        Some(
          TestCompiler.DefaultVersion
            .compileAsset(
              s"""
                 |match tx {
                 |  case _: BurnTransaction => false
                 |  case _ => true
                 |}
         """.stripMargin
            )
            .bytes()
            .base64
        ),
        waitForTx = true
      )
      .id

    assertApiError(sender.burn(firstKeyPair, unBurnable, 10, smartMinFee).id, errNotAllowedByTokenApiError)
  }

  test("masstransfer - taxation") {
    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case m:  MassTransferTransaction =>
           |  let twoTransfers = size(m.transfers) == 2
           |  let issuerIsRecipient = m.transfers[0].recipient == addressFromString("$firstAddress")
           |  let taxesPaid = m.transfers[0].amount >= m.transfers[1].amount / 10
           |  twoTransfers && issuerIsRecipient && taxesPaid
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    val transfers       = List(Transfer(firstAddress, 10), Transfer(secondAddress, 100))
    val massTransferFee = calcMassTransferFee(transfers.size)
    sender.massTransfer(firstKeyPair, transfers, massTransferFee + smartFee, assetId = Some(asset), waitForTx = true)

    val transfers2 = List(Transfer(firstAddress, 9), Transfer(secondAddress, 100))
    assertApiError(sender.massTransfer(firstKeyPair, transfers2, massTransferFee + smartFee, assetId = Some(asset)), errNotAllowedByTokenApiError)
  }

  test("masstransfer - transferCount <=2") {
    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case m:  MassTransferTransaction =>
           |  m.transferCount <= 2
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    val transfers                  = List(Transfer(firstAddress, 10), Transfer(secondAddress, 100), Transfer(firstAddress, 10))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    assertApiError(
      sender.massTransfer(firstKeyPair, transfers, massTransferTransactionFee + smartFee, assetId = Some(asset)),
      errNotAllowedByTokenApiError
    )
  }

  test("reissue by non-issuer") {
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scriptBase64), waitForTx = true)

    assertApiError(sender.reissue(secondKeyPair, asset, someAssetAmount, reissuable = true, fee = issueFee + smartFee)) { error =>
      error.message should include("Reason: Asset was issued by other address")
    }

    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case r:  ReissueTransaction => r.sender == addressFromPublicKey(base58'${secondKeyPair.publicKey}')
           |  case _ => false
           |}
         """.stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(asset, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertApiError(sender.reissue(secondKeyPair, asset, someAssetAmount, reissuable = true, fee = issueFee + smartFee)) { error =>
      error.message should include("Reason: Asset was issued by other address")
    }
  }

  test("reissue by issuer and non-issuer non-re issuable smart asset ") {
    val assetNonReissue = sender
      .issue(
        firstKeyPair,
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

    val scr = TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case r:  ReissueTransaction => r.sender == addressFromPublicKey(base58'${secondKeyPair.publicKey}')
           |  case _ => false
           |}""".stripMargin
      )
      .bytes()
      .base64
    sender.setAssetScript(assetNonReissue, firstKeyPair, setAssetScriptFee + smartFee, Some(scr), waitForTx = true)

    assertApiError(
      sender.reissue(secondKeyPair, assetNonReissue, someAssetAmount, reissuable = true, fee = issueFee + smartFee),
      AssertiveApiError(112, "State check failed. Reason: Asset was issued by other address")
    )

    assertApiError(
      sender.reissue(firstKeyPair, assetNonReissue, someAssetAmount, reissuable = true, fee = issueFee + smartFee),
      AssertiveApiError(112, "State check failed. Reason: Asset is not reissuable")
    )
  }

  test("try to send transactions forbidden by the asset's script") {
    val assetWOSupport = sender
      .issue(
        firstKeyPair,
        "assetWOSuppor",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        script = Some(TestCompiler.DefaultVersion.compileAsset(s"false".stripMargin).bytes().base64),
        waitForTx = true
      )
      .id

    assertApiError(sender.setAssetScript(assetWOSupport, firstKeyPair, setAssetScriptFee, Some(scriptBase64)), errNotAllowedByTokenApiError)
    assertApiError(sender.transfer(firstKeyPair, secondAddress, 100, smartMinFee, Some(assetWOSupport)), errNotAllowedByTokenApiError)
    assertApiError(sender.burn(firstKeyPair, assetWOSupport, 10, smartMinFee), errNotAllowedByTokenApiError)
    assertApiError(
      sender.reissue(firstKeyPair, assetWOSupport, someAssetAmount, true, issueFee + smartFee),
      AssertiveApiError(112, "State check failed. Reason: Asset is not reissuable")
    )

    val transfers = List(Transfer(firstAddress, 10))
    assertApiError(
      sender.massTransfer(firstKeyPair, transfers, calcMassTransferFee(transfers.size) + smartFee, assetId = Some(assetWOSupport)),
      errNotAllowedByTokenApiError
    )

  }

}
