package com.wavesplatform.lang.v1.estimator
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class FunctionComplexityTest extends PropSpec with PropertyChecks with Matchers with NoShrink {
  val directives: Iterable[DirectiveSet] =
  DirectiveDictionary[StdLibVersion].all
      .flatMap(
        version =>
          Seq(
            DirectiveSet(version, Account, Expression).explicitGet(),
            DirectiveSet(version, Asset, Expression).explicitGet()
          ) ++ (if (version >= V3) Seq(DirectiveSet(version, Account, DApp).explicitGet())
                else Seq())
      )

  def docCost(function: BaseFunction[Environment], version: StdLibVersion): Int =
    DocSource.funcData
      .getOrElse(
        (
          function.name,
          function.signature.args.map(_._2.toString).toList,
          version.id
        ),
        throw new Exception(s"Function ${function.name}(${function.signature.args.map(_._2.toString).toList.mkString(", ")}) not found in $version")
      )
      ._3

  property("all functions complexities") {
    directives.foreach { ds =>
      val ctx = lazyContexts(ds).value()
      ctx.functions
        .filterNot(_.name.startsWith("_"))
        .foreach { function =>
          val expr = FUNCTION_CALL(function.header, List.fill(function.args.size)(Terms.TRUE))
          val estimatedCost = ScriptEstimatorV3(
            varNames(ds.stdLibVersion, ds.contentType),
            functionCosts(ds.stdLibVersion, ds.contentType),
            expr
          ).explicitGet() - function.args.size

          val expectedCost = docCost(function, ds.stdLibVersion)

          if (estimatedCost != expectedCost)
            throw new TestFailedException(
              s"Estimated complexity = $estimatedCost is not equal to doc complexity = $expectedCost for ${ds.stdLibVersion} $function",
              0
            )
        }
    }
  }
}
