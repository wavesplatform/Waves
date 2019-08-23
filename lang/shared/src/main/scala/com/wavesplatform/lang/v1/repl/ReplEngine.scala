package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.compiler.Types.UNIT
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, FunctionTypeSignature, LazyVal}
import com.wavesplatform.lang.v1.parser.Parser

import cats.data.EitherT
import cats.implicits._
import scala.util.Try

object ReplEngine {
  def eval(
     expr:       String,
     compileCtx: CompilerContext,
     evalCtx:    EvaluationContext
  ): Either[String, (String, (CompilerContext, EvaluationContext))] =
    for {
      parsed <- Parser.parseExprOrDecl(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )
      (newCompileCtx, compiled, exprType) <- tryEi(ExpressionCompiler.applyWithCtx(compileCtx, parsed))
      (newEvalCtx, eval)                  <- tryEi(EvaluatorV1.applyWithCtx(evalCtx, compiled))
      resultO = assignedResult(exprType, eval, newCompileCtx)
      output = mkOutput(resultO, compileCtx, newCompileCtx)
      newCtx = resultO.fold((newCompileCtx, newEvalCtx))(r => addResultToCtx(r, newCompileCtx, newEvalCtx))
    } yield (output, newCtx)

  private def tryEi[R](r: => Either[String, R]): Either[String, R] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatten

  private val assignPrefix = "res"
  private val assignedR = s"^$assignPrefix([1-9][0-9]*)".r

  private def assignedResult(
    exprType:   FINAL,
    value:      EVALUATED,
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

  private val declPrefix = "defined "

  private def mkOutput(
    resultO:       Option[(String, FINAL, EVALUATED)],
    compileCtx:    CompilerContext,
    newCompileCtx: CompilerContext
  ): String = {
    val (lets, funcs) = declsDiff(compileCtx, newCompileCtx)

    val mappedFuncs =
      for {
        (name, overloads) <- funcs
        signature         <- overloads
      } yield declPrefix + DeclPrinter.funcStr(name, signature)

    val mappedLets =
      lets.map { case (name, t) => declPrefix + DeclPrinter.letStr(name, t) }

    val evalStr =
      resultO.fold("") { case (name, t, result) => s"$name: $t = $result" }

    val delim1 = if (mappedLets.nonEmpty && mappedFuncs.nonEmpty) "\n" else ""
    val delim2 = if ((mappedLets.nonEmpty || mappedFuncs.nonEmpty) && resultO.isDefined) "\n" else ""

    mappedFuncs.mkString("\n") + delim1 + mappedLets.mkString("\n") + delim2 + evalStr
  }

  private def declsDiff(
    compileCtx:    CompilerContext,
    newCompileCtx: CompilerContext
  ): (Set[(String, FINAL)], Set[(String, List[FunctionTypeSignature])]) = {
    val newLets =
      (newCompileCtx.varDefs.keySet diff compileCtx.varDefs.keySet)
        .map(n => (n, newCompileCtx.varDefs(n)))

    val newFuncs =
      (newCompileCtx.functionDefs.keySet diff compileCtx.functionDefs.keySet)
        .map(n => (n, newCompileCtx.functionDefs(n)))

    (newLets, newFuncs)
  }

  private def addResultToCtx(
    result:     (String, FINAL, EVALUATED),
    compileCtx: CompilerContext,
    evalCtx:    EvaluationContext
  ): (CompilerContext, EvaluationContext) = {
    val (name, t, value) = result
    (
      compileCtx.copy(varDefs = compileCtx.varDefs + (name -> t)),
      evalCtx.copy(letDefs = evalCtx.letDefs + (name -> LazyVal(EitherT.pure(value))))
    )
  }
}
