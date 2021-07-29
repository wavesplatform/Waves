package com.wavesplatform.state.diffs

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.TransactionType
import org.scalacheck.Gen

package object ci {
  private val invokeFee = FeeUnit * FeeConstants(TransactionType.InvokeScript)

  def ciFee(sc: Int = 0, nonNftIssue: Int = 0): Gen[Long] =
    Gen.choose(
      invokeFee + sc * ScriptExtraFee + nonNftIssue * FeeConstants(TransactionType.Issue) * FeeUnit,
      invokeFee + (sc + 1) * ScriptExtraFee - 1 + nonNftIssue * FeeConstants(TransactionType.Issue) * FeeUnit
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
