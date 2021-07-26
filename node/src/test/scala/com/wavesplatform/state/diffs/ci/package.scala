package com.wavesplatform.state.diffs

import com.wavesplatform.account.KeyPair
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{V3, V5}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import org.scalacheck.Gen
import com.wavesplatform.common.utils.EitherExt2


package object ci {
  private def invokeFee(freeCall: Boolean) =
    if (freeCall)
      FeeUnit * FeeConstants(InvokeExpressionTransaction.typeId)
    else
      FeeUnit * FeeConstants(InvokeScriptTransaction.typeId)

  def ciFee(sc: Int = 0, nonNftIssue: Int = 0, freeCall: Boolean = false): Gen[Long] =
    Gen.choose(
      invokeFee(freeCall) + sc * ScriptExtraFee + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit,
      invokeFee(freeCall) + (sc + 1) * ScriptExtraFee - 1 + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit
    )

  def simpleContract(funcName: String): Either[String, DApp] =
    TestCompiler(V3).compile(
      s"""
         |{-# STDLIB_VERSION 3 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         |@Callable(xx)
         |func $funcName(str: String, num: Int) = {
         |    if (parseInt(str) == num) then throw() else throw()
         |}
         |
         |@Verifier(txx)
         |func verify() = {
         |    false
         |}
       """.stripMargin
    )

  def toInvokeExpression(setScript: SetScriptTransaction, invoker: KeyPair, fee: Option[Long] = None): InvokeExpressionTransaction = {
    val expression = setScript.script.get.asInstanceOf[ContractScriptImpl].expr.callableFuncs.head.u.body
    InvokeExpressionTransaction
      .selfSigned(
        TxVersion.V1,
        invoker,
        ExprScript(V5, expression, isFreeCall = true).explicitGet(),
        fee.getOrElse(ciFee(freeCall = true).sample.get),
        Waves,
        setScript.timestamp
      )
      .explicitGet()
  }
}
