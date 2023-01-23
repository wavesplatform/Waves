package com.wavesplatform.lang.v1.repl

import cats.Monad
import cats.data.EitherT
import cats.implicits.*
import com.wavesplatform.lang.v1.compiler.CompilerContext.VariableInfo
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler, TermPrinter}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, FunctionTypeSignature, LazyVal}
import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.repl.Implicits.*
import com.wavesplatform.lang.v1.traits.Environment

class ReplEngine[F[_]: Monad] {
  val evaluator = new EvaluatorV1[F, Environment]

  def eval(
      expr: String,
      compileCtx: CompilerContext,
      evalCtx: EvaluationContext[Environment, F]
  ): F[Either[String, (String, (CompilerContext, EvaluationContext[Environment, F]))]] = {
    val r =
      for {
        parsed                              <- EitherT.fromEither[F](parse(expr))
        (newCompileCtx, compiled, exprType) <- EitherT.fromEither[F](ExpressionCompiler.applyWithCtx(compileCtx, parsed))
        evaluated <- EitherT(evaluator.applyWithCtx(evalCtx, compiled)).leftMap(error =>
          if (error.message.isEmpty) "Evaluation error" else error.message
        )
      } yield resultWithCtx(evaluated, compileCtx, newCompileCtx, exprType)

    r.value
  }

  private def parse(expr: String): Either[String, EXPR] =
    Parser
      .parseReplExpr(expr)
      .fold(
        { case _ => Left(s"Can't parse '$expr'") },
        { case (result, _) => Right(result) }
      )

  private def resultWithCtx(
      evaluated: (EvaluationContext[Environment, F], EVALUATED),
      compileCtx: CompilerContext,
      newCompileCtx: CompilerContext,
      exprType: FINAL
  ) = {
    val (newEvalCtx, result) = evaluated
    val filteredCompileCtx   = excludeInternalDecls(newCompileCtx)
    val resultO              = assignedResult(exprType, result, filteredCompileCtx)
    val output               = mkOutput(resultO, compileCtx, filteredCompileCtx)
    val newCtx               = resultO.fold((filteredCompileCtx, newEvalCtx))(addResultToCtx(_, filteredCompileCtx, newEvalCtx))
    (output, newCtx)
  }

  private def excludeInternalDecls(compileCtx: CompilerContext) =
    compileCtx.copy(varDefs = compileCtx.varDefs.filterNot(v => internalVarPrefixes.contains(v._1.head)))

  private val assignPrefix = "res"
  private val assignedR    = s"^$assignPrefix([1-9][0-9]*)".r

  private def assignedResult(
      exprType: FINAL,
      value: EVALUATED,
      compileCtx: CompilerContext
  ): Option[(String, FINAL, EVALUATED)] =
    if (exprType == UNIT) None
    else {
      val count =
        compileCtx.varDefs.keys
          .flatMap(assignedR.findPrefixMatchOf(_).toSeq)
          .map(_.group(1).toInt)
          .toList
          .maximumOption
          .getOrElse(0)

      val name = assignPrefix + (count + 1)
      Some((name, exprType, value))
    }

  private def mkOutput(
      resultO: Option[(String, FINAL, EVALUATED)],
      compileCtx: CompilerContext,
      newCompileCtx: CompilerContext
  ): String = {
    val (lets, funcs) = declsDiff(compileCtx, newCompileCtx)

    val mappedFuncs =
      for {
        (name, overloads) <- funcs.toSeq.sortBy(_._1)
        signature         <- overloads
      } yield DeclPrinter.declaredFuncStr(name, signature)

    val mappedLets =
      lets.toSeq.sortBy(_._1).map { case (name, t) => DeclPrinter.declaredLetStr(name, t) }

    val evalStr =
      resultO.fold("") { case (name, t, result) => s"$name: $t = ${TermPrinter().prettyString(result, 0)}" }

    val delim1 = if (mappedLets.nonEmpty && mappedFuncs.nonEmpty) "\n" else ""
    val delim2 = if ((mappedLets.nonEmpty || mappedFuncs.nonEmpty) && resultO.isDefined) "\n" else ""

    mappedFuncs.mkString("\n") + delim1 + mappedLets.mkString("\n") + delim2 + evalStr
  }

  private def declsDiff(
      compileCtx: CompilerContext,
      newCompileCtx: CompilerContext
  ): (Set[(String, FINAL)], Set[(String, List[FunctionTypeSignature])]) = {
    val newLets =
      (newCompileCtx.varDefs.keySet diff compileCtx.varDefs.keySet)
        .map(n => (n, newCompileCtx.varDefs(n).vType))

    val newFuncs =
      (newCompileCtx.functionDefs.keySet diff compileCtx.functionDefs.keySet)
        .map(n => (n, newCompileCtx.functionDefs(n).fSigList))

    (newLets, newFuncs)
  }

  private def addResultToCtx(
      result: (String, FINAL, EVALUATED),
      compileCtx: CompilerContext,
      evalCtx: EvaluationContext[Environment, F]
  ): (CompilerContext, EvaluationContext[Environment, F]) = {
    val (name, t, value) = result
    (
      compileCtx.copy(varDefs = compileCtx.varDefs + (name -> VariableInfo(AnyPos, t))),
      evalCtx.copy(letDefs = evalCtx.letDefs + (name -> LazyVal.fromEvaluated(value)))
    )
  }
}
