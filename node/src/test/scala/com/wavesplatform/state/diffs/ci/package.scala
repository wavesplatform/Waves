package com.wavesplatform.state.diffs

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction}
import org.scalacheck.Gen

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

}
