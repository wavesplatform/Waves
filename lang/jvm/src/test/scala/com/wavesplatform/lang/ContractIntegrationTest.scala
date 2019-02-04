package com.wavesplatform.lang

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invokation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.domain.DataItem
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

  property("Simple test") {
    val ctx: CTX =
      PureContext.build(Version.ContractV) |+|
        CTX(sampleTypes, Map.empty, Array.empty) |+|
        WavesContext.build(Version.ContractV, Common.emptyBlockchainEnvironment(), false)

    val src =
      """
        |
        |func fooHelper2() = {
        |   false
        |}
        |
        |func fooHelper() = {
        |   fooHelper2() || false
        |}
        |
        |@Callable(invocation)
        |func foo(a:ByteStr) = {
        |  let x = invocation.caller.bytes
        |  if (fooHelper())
        |    then WriteSet(List(DataEntry("b", 1), DataEntry("sender", x)))
        |    else WriteSet(List(DataEntry("a", a), DataEntry("sender", x)))
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin

    val parsed = Parser.parseContract(src).get.value

    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    val expectedResult = ContractResult(
      List(
        DataItem.Bin("a", ByteStr.empty),
        DataItem.Bin("sender", ByteStr.empty)
      ),
      List()
    )

    val result = ContractEvaluator(
      ctx.evaluationContext,
      compiled,
      Invokation(Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_BYTESTR(ByteStr.empty))), ByteStr.empty, None, ByteStr.empty)
    ).explicitGet()

    result shouldBe expectedResult
  }

}
