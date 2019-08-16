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
      (resultNameO, newCtx) = assignResult(exprType, eval, newCompileCtx, newEvalCtx)
      (lets, funcs) = declsDiff(compileCtx, newCompileCtx)
      resultO = resultNameO.map((_, exprType, eval))
      output = mkOutput(resultO, lets, funcs)
    } yield (output, newCtx)

  private def tryEi[R](r: => Either[String, R]): Either[String, R] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatten

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

  private val assignPrefix = "res"
  private val assignedR = s"^$assignPrefix([1-9][0-9]*)".r

  private def assignResult(
    exprType:   FINAL,
    value:      EVALUATED,
    compileCtx: CompilerContext,
    evalCtx:    EvaluationContext
  ): (Option[String], (CompilerContext, EvaluationContext)) =
    if (exprType == UNIT) (None, (compileCtx, evalCtx))
    else {
      val count =
        compileCtx.varDefs.keys
          .flatMap(assignedR.findPrefixMatchOf(_).toSeq)
          .map(_.group(1).toInt)
          .toList
          .maximumOption
          .getOrElse(0)

      val nextName = assignPrefix + (count + 1)
      val newCtx = (
        compileCtx.copy(varDefs = compileCtx.varDefs + (nextName -> exprType)),
        evalCtx.copy(letDefs = evalCtx.letDefs + (nextName -> LazyVal(EitherT.pure(value))))
      )
      (Some(nextName), newCtx)
    }

  private val declPrefix = "defined "

  private def mkOutput(
    resultO: Option[(String, FINAL, EVALUATED)],
    lets:    Set[(String, FINAL)],
    funcs:   Set[(String, List[FunctionTypeSignature])]
  ): String = {
    val mappedFuncs = for {
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
}
