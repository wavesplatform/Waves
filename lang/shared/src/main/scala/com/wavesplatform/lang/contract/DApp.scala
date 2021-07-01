package com.wavesplatform.lang.contract

import cats.Eval
import cats.implicits._
import com.wavesplatform.lang.contract.DApp.{CallableFunction, VerifierFunction}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilationError, Terms}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{CALLDAPP, CALLDAPPREENTRANT}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.protobuf.dapp.DAppMeta

case class DApp(
    meta: DAppMeta,
    decs: List[DECLARATION],
    callableFuncs: List[CallableFunction],
    verifierFuncOpt: Option[VerifierFunction]
) {
  val containsSyncCall: Boolean =
    verifierFuncOpt.map(_.u.body).traverse(containsSyncCall(_)).value.getOrElse(false)

  private def containsSyncCall(e: EXPR): Eval[Boolean] = {
    def contains2(e1: EXPR, e2: EXPR): Eval[Boolean] =
      containsEval2(containsSyncCall(e1), containsSyncCall(e2))

    def containsEval2(e1: Eval[Boolean], e2: Eval[Boolean]): Eval[Boolean] =
      for {
        r1 <- e1
        r2 <- e2
      } yield r1 || r2

    def contains3(e1: EXPR, e2: EXPR, e3: EXPR): Eval[Boolean] =
      for {
        r1 <- containsSyncCall(e1)
        r2 <- containsSyncCall(e2)
        r3 <- containsSyncCall(e3)
      } yield r1 || r2 || r3

    def checkFunctionBody(header: FunctionHeader) =
      header match {
        case Native(_) => Eval.now(false)
        case FunctionHeader.User(_, name) =>
          decs
            .collectFirst {
              case FUNC(`name`, _, body) => containsSyncCall(body)
            }
            .getOrElse(Eval.now(false))
      }

    def checkFunctionCall(header: FunctionHeader, args: List[EXPR]) = {
      val checkBody = checkFunctionBody(header)
      val checkArgs = args.traverse(containsSyncCall(_)).map(_.exists(identity))
      containsEval2(checkBody, checkArgs)
    }

    def checkRef(key: String): Eval[Boolean] =
      decs
        .collectFirst {
          case LET(`key`, body) => containsSyncCall(body)
        }
        .getOrElse(Eval.now(false))

    e match {
      case GETTER(expr, _)                             => containsSyncCall(expr)
      case LET_BLOCK(LET(_, value), body)              => contains2(value, body)
      case BLOCK(LET(_, value), body)                  => contains2(value, body)
      case BLOCK(FUNC(_, _, value), body)              => contains2(value, body)
      case IF(cond, ifTrue, ifFalse)                   => contains3(cond, ifTrue, ifFalse)
      case FUNCTION_CALL(Native(CALLDAPP), _)          => Eval.now(true)
      case FUNCTION_CALL(Native(CALLDAPPREENTRANT), _) => Eval.now(true)
      case FUNCTION_CALL(header, args)                 => checkFunctionCall(header, args)
      case REF(key)                                    => checkRef(key)
      case _                                           => Eval.now(false)
    }
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
}
