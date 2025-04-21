package com.wavesplatform.lang.contract

import com.wavesplatform.lang.contract.DApp.{CallableFunction, ExprWithCtx, VerifierFunction}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.compiler.{CompilationError, Terms}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{CALLDAPP, CALLDAPPREENTRANT}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.protobuf.dapp.DAppMeta

import scala.annotation.tailrec

case class DApp(
    meta: DAppMeta,
    decs: List[DECLARATION],
    callableFuncs: List[CallableFunction],
    verifierFuncOpt: Option[VerifierFunction]
) {
  def verifierContainsSyncCall: Boolean =
    verifierFuncOpt.map(_.u.body).exists { e =>
      containsSyncCall(List(ExprWithCtx(e, Set.empty)))
    }

  private def containsSyncCall(e: List[ExprWithCtx]): Boolean = {
    val commonCtx = decs
      .collect {
        case LET(name, value)    => name -> ExprWithCtx(value, Set.empty)
        case FUNC(name, _, body) => name -> ExprWithCtx(body, Set.empty)
      }
      .groupBy { case (name, _) => name }
      .view
      .mapValues(_.map { case (_, ewc) => ewc })
      .toMap

    @tailrec
    def checkLoop(e: List[ExprWithCtx], checked: Set[ExprWithCtx]): Boolean =
      e match {
        case Nil                                                              => false
        case ExprWithCtx(FUNCTION_CALL(Native(CALLDAPP), _), _) :: _          => true
        case ExprWithCtx(FUNCTION_CALL(Native(CALLDAPPREENTRANT), _), _) :: _ => true
        case ewc :: l if checked.contains(ewc)                                => checkLoop(l, checked)
        case (ewc @ ExprWithCtx(GETTER(expr, _), ctx)) :: l =>
          checkLoop(ExprWithCtx(expr, ctx) :: l, checked + ewc)
        case (ewc @ ExprWithCtx(LET_BLOCK(LET(name, value), body), ctx)) :: l =>
          checkLoop(ExprWithCtx(value, ctx) :: ExprWithCtx(body, ctx + name) :: l, checked + ewc)
        case (ewc @ ExprWithCtx(BLOCK(LET(name, value), body), ctx)) :: l =>
          checkLoop(ExprWithCtx(value, ctx) :: ExprWithCtx(body, ctx + name) :: l, checked + ewc)
        case (ewc @ ExprWithCtx(BLOCK(FUNC(name, _, value), body), ctx)) :: l =>
          checkLoop(ExprWithCtx(value, ctx) :: ExprWithCtx(body, ctx + name) :: l, checked + ewc)
        case (ewc @ ExprWithCtx(IF(cond, ifTrue, ifFalse), ctx)) :: l =>
          checkLoop(ExprWithCtx(cond, ctx) :: ExprWithCtx(ifTrue, ctx) :: ExprWithCtx(ifFalse, ctx) :: l, checked + ewc)
        case (ewc @ ExprWithCtx(FUNCTION_CALL(header, args), ctx)) :: l =>
          val maybeBody = if (ctx.contains(header.funcName)) {
            List.empty
          } else {
            commonCtx.get(header.funcName).toList.flatten
          }
          checkLoop(maybeBody ::: args.map(ExprWithCtx(_, ctx)) ::: l, checked + ewc)
        case (ewc @ ExprWithCtx(REF(key), ctx)) :: l =>
          val maybeBody = if (ctx.contains(key)) {
            List.empty
          } else {
            commonCtx.get(key).toList.flatten
          }
          checkLoop(maybeBody ::: l, checked + ewc)
        case ewc :: l => checkLoop(l, checked + ewc)
      }

    checkLoop(e, Set.empty)
  }
}

object DApp {

  sealed trait Annotation {
    def invocationArgName: String
    def dic(version: StdLibVersion): Map[String, FINAL]
  }
  object Annotation {
    def parse(name: String, args: List[String]): Either[CompilationError, Annotation] = {
      (name, args) match {
        case ("Verifier", s :: Nil) => Right(VerifierAnnotation(s))
        case ("Verifier", s :: xs)  => Left(Generic(0, 0, "Incorrect amount of bound args in Verifier, should be one, e.g. @Verifier(tx)"))
        case ("Callable", s :: Nil) => Right(CallableAnnotation(s))
        case ("Callable", s :: xs)  => Left(Generic(0, 0, "Incorrect amount of bound args in Callable, should be one, e.g. @Callable(inv)"))
        case _                      => Left(Generic(0, 0, "Annotation not recognized"))
      }
    }

    def validateAnnotationSet(l: List[Annotation]): Either[CompilationError, Unit] = {
      l match {
        case (v: VerifierAnnotation) :: Nil => Right(())
        case (c: CallableAnnotation) :: Nil => Right(())
        case _                              => Left(Generic(0, 0, "Unsupported annotation set"))
      }
    }
  }
  case class CallableAnnotation(invocationArgName: String) extends Annotation {
    override def dic(version: StdLibVersion): Map[String, FINAL] =
      Map(invocationArgName -> Types.invocationType(version))
  }

  case class VerifierAnnotation(invocationArgName: String) extends Annotation {
    override def dic(version: StdLibVersion): Map[String, FINAL] =
      Map(invocationArgName -> Types.verifierInput(version))
  }

  sealed trait AnnotatedFunction {
    def annotation: Annotation
    def u: Terms.FUNC
  }
  case class CallableFunction(override val annotation: CallableAnnotation, override val u: Terms.FUNC) extends AnnotatedFunction
  case class VerifierFunction(override val annotation: VerifierAnnotation, override val u: Terms.FUNC) extends AnnotatedFunction

  case class ExprWithCtx(expr: EXPR, ctx: Set[String])
}
