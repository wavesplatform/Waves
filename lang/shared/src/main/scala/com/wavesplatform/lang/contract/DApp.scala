package com.wavesplatform.lang.contract

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

import scala.annotation.tailrec

case class DApp(
    meta: DAppMeta,
    decs: List[DECLARATION],
    callableFuncs: List[CallableFunction],
    verifierFuncOpt: Option[VerifierFunction]
) {
  val verifierContainsSyncCall: Boolean =
    verifierFuncOpt.map(_.u.body).exists(e => containsSyncCall(List(e)))

  @tailrec private def containsSyncCall(e: List[EXPR]): Boolean = {
    def funcBody(header: FunctionHeader): List[EXPR] =
      decs.collectFirst { case FUNC(header.funcName, _, body) => body }.toList

    def refBody(key: String): List[EXPR] =
      decs.collect { case LET(`key`, body) => body }

    e match {
      case Nil                                              => false
      case FUNCTION_CALL(Native(CALLDAPP), _) :: _          => true
      case FUNCTION_CALL(Native(CALLDAPPREENTRANT), _) :: _ => true
      case GETTER(expr, _) :: l                             => containsSyncCall(expr :: l)
      case LET_BLOCK(LET(_, value), body) :: l              => containsSyncCall(value :: body :: l)
      case BLOCK(LET(_, value), body) :: l                  => containsSyncCall(value :: body :: l)
      case BLOCK(FUNC(_, _, value), body) :: l              => containsSyncCall(value :: body :: l)
      case IF(cond, ifTrue, ifFalse) :: l                   => containsSyncCall(cond :: ifTrue :: ifFalse :: l)
      case FUNCTION_CALL(header, args) :: l                 => containsSyncCall(funcBody(header) ::: args ::: l)
      case REF(key) :: l                                    => containsSyncCall(refBody(key) ::: l)
      case _ :: l                                           => containsSyncCall(l)
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
