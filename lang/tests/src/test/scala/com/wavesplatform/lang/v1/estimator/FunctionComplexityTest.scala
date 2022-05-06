package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.test.PropSpec
import org.scalatest.exceptions.TestFailedException

class FunctionComplexityTest extends PropSpec {
  private val directives: Iterable[DirectiveSet] =
    DirectiveDictionary[StdLibVersion].all
      .flatMap(version =>
        Seq(
          DirectiveSet(version, Account, Expression).explicitGet(),
          DirectiveSet(version, Asset, Expression).explicitGet()
        ) ++ (if (version >= V3) Seq(DirectiveSet(version, Account, DApp).explicitGet())
              else Seq())
      )

  private val dAppOnlyFunctions =
    Set("invoke", "reentrantInvoke")

  private val baseDataStorageFunctions =
    Set("getString", "getBoolean", "getBinary", "getInteger")

  private val allDataStorageFunctions =
    baseDataStorageFunctions ++ baseDataStorageFunctions.map(_ + "Value")

  private def check(functions: Array[BaseFunction[Environment]], ds: DirectiveSet): Unit = {
    val docCosts =
      DocSource.funcData.collect {
        case ((name, signature, version), (_, _, complexity)) if version == ds.stdLibVersion.id => ((name, signature), complexity)
      }.toMap
    val unusedDocCosts =
      functions
        .filterNot(_.name.startsWith("$"))
        .foldLeft(docCosts) { case (remainingDocCosts, function) =>
          val arg  = CONST_STRING("throw").explicitGet()
          val expr = FUNCTION_CALL(function.header, List.fill(function.args.size)(arg))
          val estimatedCost =
            ScriptEstimatorV3(fixOverflow = true, overhead = false)(
              varNames(ds.stdLibVersion, ds.contentType),
              functionCosts(ds.stdLibVersion, ds.contentType),
              expr
            ).explicitGet()

          val name = function.name
          val args = function.signature.args.map(_._2.toString).toList
          val expectedCost =
            remainingDocCosts.getOrElse(
              (name, args),
              throw new TestFailedException(s"Function $name(${args.mkString(", ")}) not found for RIDE ${ds.stdLibVersion}", 0)
            )

          if (estimatedCost != expectedCost)
            throw new TestFailedException(
              s"Estimated complexity = $estimatedCost is not equal to doc complexity = $expectedCost for ${ds.stdLibVersion} $function",
              0
            )
          remainingDocCosts - ((name, args))
        }

    def onlyDApp(costs: Map[(String, List[String]), Int]) =
      if (ds.contentType != DApp)
        costs.filterNot(c => dAppOnlyFunctions.contains(c._1._1))
      else
        costs

    def onlyAccount(costs: Map[(String, List[String]), Int]) =
      if (ds.scriptType == Asset)
        costs.filterNot { case ((name, args), _) => allDataStorageFunctions.contains(name) && args.size == 1 }
      else
        costs

    val checkedUnusedDocCosts = onlyDApp(onlyAccount(unusedDocCosts))
    if (checkedUnusedDocCosts.nonEmpty)
      throw new TestFailedException(
        s"For RIDE ${ds.stdLibVersion} documented functions is unused: ${checkedUnusedDocCosts.map(_._1).mkString(", ")}",
        0
      )
  }

  property("all functions complexities") {
    val contexts = lazyContexts
      .groupBy { case ((directiveSet, _), _) => directiveSet }
      .view
      .mapValues(_.map { case (_, context) => context })
      .toMap

    directives
      .flatMap(ds => contexts(ds).map(ds -> _))
      .foreach { case (ds, context) => check(context.value().functions, ds) }
  }
}
