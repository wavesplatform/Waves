package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.it.api.SyncHttpApi.{assertBadRequestAndResponse, _}
import com.wavesplatform.it.sync.minFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.{CompilerV1, Terms}
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.utils.dummyCompilerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber

class ScriptExecutionErrorSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  test("custom throw message") {
    val scriptSrc =
      """
        |match tx {
        |  case t : TransferTransaction =>
        |    let res = if isDefined(t.assetId) then extract(t.assetId) == base58'' else isDefined(t.assetId) == false
        |    res
        |  case t : SetScriptTransaction => true
        |  case other => throw("Your transaction has incorrect type.")
        |}
      """.stripMargin

    val compiler = new CompilerV1(dummyCompilerContext)

    val compiled =
      ScriptV1(
        compiler
          .compile(scriptSrc, Nil)
          .explicitGet(),
        false
      ).explicitGet()

    val tx = sender.signedBroadcast(
      SetScriptTransaction
        .selfSigned(
          1,
          acc2,
          Some(compiled),
          minFee,
          System.currentTimeMillis()
        )
        .explicitGet()
        .json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))

    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(sender.createAlias(acc2.address, "asdasdasdv", minFee), "Your transaction has incorrect type.")
  }

  test("wrong type of script return value") {
    val script = ScriptV1(
      Terms
        .FUNCTION_CALL(
          FunctionHeader.Native(100),
          List(
            Terms.CONST_LONG(3),
            Terms.CONST_LONG(2)
          )
        )
    ).explicitGet()

    val tx = sender
      .signAndBroadcast(
        SetScriptTransaction
          .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), minFee, System.currentTimeMillis())
          .explicitGet()
          .json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(tx)

    assertBadRequestAndResponse(
      sender.transfer(acc0.address, acc1.address, 1000, minFee, None, None),
      "Probably script does not return boolean"
    )
  }
}
