package com.wavesplatform.lang.v1.repl

import cats.{Id, Monad}
import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, FunctionTypeSignature, LazyVal}
import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.lang.v1.parser.Parser

import scala.util.Try

class ReplEngine[F[_] : Monad] {
  val evaluator = new EvaluatorV1[F]

  def eval(
     expr:       String,
     compileCtx: CompilerContext,
     evalCtx:    EvaluationContext[F]
  ): F[Either[String, (String, (CompilerContext, EvaluationContext[F]))]] = {
    val r = for {
      parsed                              <- EitherT.fromEither[F](parse(expr))
      (newCompileCtx, compiled, exprType) <- EitherT.fromEither[F](tryEi[Id,  (CompilerContext, Terms.EXPR, FINAL)](ExpressionCompiler.applyWithCtx(compileCtx, parsed)))
      evaluated                           <- EitherT(tryEi(evaluator.applyWithCtx(evalCtx, compiled)))
      result = resultWithCtx(evaluated, compileCtx, newCompileCtx, exprType)
    } yield result
    r.value
  }

  private def parse(expr: String): Either[String, EXPR] =
    Parser.parseExprOrDecl(expr)
      .fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )

  private def resultWithCtx(
    evaluated:     (EvaluationContext[F], EVALUATED),
    compileCtx:    CompilerContext,
    newCompileCtx: CompilerContext,
    exprType:      FINAL
  ) = {
      val (newEvalCtx, result) = evaluated
      val filteredCompileCtx = excludeInternalDecls(newCompileCtx)
      val resultO = assignedResult(exprType, result, filteredCompileCtx)
      val output = mkOutput(resultO, compileCtx, filteredCompileCtx)
      val newCtx = resultO.fold((filteredCompileCtx, newEvalCtx))(addResultToCtx(_, filteredCompileCtx, newEvalCtx))
      (output, newCtx)
    }

  private def tryEi[M[_] : Monad, R](r: => M[Either[String, R]]): M[Either[String, R]] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatSequence

  private def excludeInternalDecls(compileCtx: CompilerContext) =
    compileCtx.copy(varDefs = compileCtx.varDefs.filterNot(v => internalVarPrefixes.contains(v._1.head)))

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
      } yield DeclPrinter.declaredFuncStr(name, signature)

    val mappedLets =
      lets.map { case (name, t) => DeclPrinter.declaredLetStr(name, t) }

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
    evalCtx:    EvaluationContext[F]
  ): (CompilerContext, EvaluationContext[F]) = {
    val (name, t, value) = result
    (
      compileCtx.copy(varDefs = compileCtx.varDefs + (name -> t)),
      evalCtx.copy(letDefs = evalCtx.letDefs + (name -> LazyVal.fromEvaluated(value)))
    )
  }
}
