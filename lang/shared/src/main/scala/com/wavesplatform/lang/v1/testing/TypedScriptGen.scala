package com.wavesplatform.lang.v1.testing

import com.wavesplatform.lang.contract.{DApp, ContractSerDe}
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import org.scalacheck._

trait TypedScriptGen {

  def exprGen = BOOLEANgen(100)
  private def letGen =
    for {
      name <- Gen.alphaStr
      expr <- exprGen
    } yield Terms.LET(name, expr)

  private def funcGen =
    for {
      name <- Gen.alphaStr.filter(_.getBytes.length <= ContractLimits.MaxCallableFunctionNameInBytes)
      arg0 <- Gen.alphaStr
      args <- Gen.listOf(Gen.alphaStr)
      allArgs = arg0 +: args
      returned <- Gen.oneOf(allArgs)
    } yield Terms.FUNC(name, allArgs, Terms.REF(returned))

  private def callableGen =
    for {
      binding <- Gen.alphaStr
      fnc     <- funcGen
    } yield CallableFunction(CallableAnnotation(binding), fnc)

  private def verifierGen =
    for {
      binding <- Gen.alphaStr
      name    <- Gen.alphaStr
      expr    <- exprGen
    } yield VerifierFunction(VerifierAnnotation(binding), Terms.FUNC(name, List.empty, expr))

  def contractGen =
    for {
      nLets      <- Gen.chooseNum(0, 5)
      nFuncs     <- Gen.chooseNum(0, 5)
      nCallables <- Gen.chooseNum(0, 5)
      lets       <- Gen.listOfN(nLets, letGen)
      funcs      <- Gen.listOfN(nFuncs, funcGen)
      callables  <- Gen.listOfN(nCallables, callableGen)
      verifier   <- Gen.option(verifierGen)
      c = DApp(lets ++ funcs, callables, verifier)
      if ContractSerDe.serialize(c).size < Short.MaxValue - 3 - 4
    } yield c

  def BOOLEANgen(gas: Int): Gen[EXPR] =
    if (gas > 0) Gen.oneOf(CONST_BOOLEANgen, BLOCK_BOOLEANgen(gas - 1), IF_BOOLEANgen(gas - 1), FUNCTION_CALLgen(BOOLEAN))
    else Gen.const(TRUE)

  private def CONST_BOOLEANgen: Gen[EXPR] = Gen.oneOf(FALSE, TRUE)

  private def BLOCK_BOOLEANgen(gas: Int): Gen[EXPR] =
    for {
      let  <- LETgen((gas - 3) / 3)
      body <- Gen.oneOf(BOOLEANgen((gas - 3) / 3), BLOCK_BOOLEANgen((gas - 3) / 3))
    } yield BLOCK(let, body)

  private def IF_BOOLEANgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLEANgen((gas - 3) / 3)
      t   <- BOOLEANgen((gas - 3) / 3)
      f   <- BOOLEANgen((gas - 3) / 3)
    } yield IF(cnd, t, f)

  private def LONGgen(gas: Int): Gen[EXPR] =
    if (gas > 0) Gen.oneOf(CONST_LONGgen, BLOCK_LONGgen(gas - 1), IF_LONGgen(gas - 1), FUNCTION_CALLgen(LONG)) else CONST_LONGgen

  private def CONST_LONGgen: Gen[EXPR] = Gen.choose(Long.MinValue, Long.MaxValue).map(CONST_LONG)

  private def BLOCK_LONGgen(gas: Int): Gen[EXPR] =
    for {
      let  <- LETgen((gas - 3) / 3)
      body <- Gen.oneOf(LONGgen((gas - 3) / 3), BLOCK_LONGgen((gas - 3) / 3))
    } yield LET_BLOCK(let, body)

  private def IF_LONGgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLEANgen((gas - 3) / 3)
      t   <- LONGgen((gas - 3) / 3)
      f   <- LONGgen((gas - 3) / 3)
    } yield IF(cnd, t, f)

  private def FUNCTION_CALLgen(resultType: TYPE): Gen[EXPR] =
    Gen.const(
      FUNCTION_CALL(
        function = FunctionHeader.Native(SUM_LONG),
        args = List(CONST_LONG(1), CONST_LONG(1))
      )
    )

  private def LETgen(gas: Int): Gen[LET] =
    for {
      name  <- Gen.identifier
      value <- BOOLEANgen((gas - 3) / 3)
    } yield LET(name, value)

}
