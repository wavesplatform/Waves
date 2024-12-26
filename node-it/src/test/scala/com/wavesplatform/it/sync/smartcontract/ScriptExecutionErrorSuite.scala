package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee, smartFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.CancelAfterFailure

class ScriptExecutionErrorSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val ts = System.currentTimeMillis()

  test("custom throw message") {
    val scriptSrc =
      """
        |match tx {
        |  case t : TransferTransaction =>
        |    let res = if isDefined(t.assetId) then extract(t.assetId) == base58'' else isDefined(t.assetId) == false
        |    res
        |  case _: SetScriptTransaction => true
        |  case _ => throw("Your transaction has incorrect type.")
        |}
      """.stripMargin

    val compiled = ScriptCompiler.compile(scriptSrc, ScriptEstimatorV2).explicitGet()._1

    val tx = sender.signedBroadcast(SetScriptTransaction.selfSigned(1.toByte, thirdKeyPair, Some(compiled), setScriptFee, ts).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    val alias = Alias.fromString(s"alias:${AddressScheme.current.chainId.toChar}:asdasdasdv").explicitGet()
    assertBadRequestAndResponse(
      sender.signedBroadcast(CreateAliasTransaction.selfSigned(Transaction.V2, thirdKeyPair, alias.name, minFee + smartFee, ts).explicitGet().json()),
      "Your transaction has incorrect type."
    )
  }

  test("wrong type of script return value") {
    val script = ExprScript(
      Terms.FUNCTION_CALL(
        FunctionHeader.Native(100),
        List(Terms.CONST_LONG(3), Terms.CONST_LONG(2))
      )
    ).explicitGet()

    val tx = sender.signedBroadcast(
      SetScriptTransaction
        .selfSigned(1.toByte, firstKeyPair, Some(script), setScriptFee, ts)
        .explicitGet()
        .json()
    )
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransaction
          .selfSigned(2.toByte, firstKeyPair, secondKeyPair.toAddress, Waves, 1000, Waves, minFee + smartFee, ByteStr.empty, ts)
          .explicitGet()
          .json()
      ),
      "not a boolean"
    )
  }
}
