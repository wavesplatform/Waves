package com.wavesplatform.lang.contract

import com.wavesplatform.lang.contract.Contract.{ContractFunction, VerifierFunction}
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.Terms.DECLARATION
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilationError, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext

/*
 Contact is a list of annotated definitions
 ContractInvokation is (ContractFunction name, ContractFunction args)

 In PoC:
  - No declarations
  - ContractFunctions can't invoke each other
 */

case class Contract(
    dec: List[DECLARATION],
    cfs: List[ContractFunction],
    vf: Option[VerifierFunction]
)

object Contract {

  sealed trait Annotation {
    def dic: Map[String, FINAL]
  }
  object Annotation {
    def parse(name: String, args: List[String]): Either[CompilationError, Annotation] = {
      (name, args) match {
        case ("Callable", s :: Nil)           => Right(CallableAnnotation(s))
        case ("Verifier", s :: Nil)           => Right(VerifierAnnotation(s))
        case _                                => Left(Generic(0, 0, "Annotation not recognized"))
      }
    }

    def validateAnnotationSet(l: List[Annotation]): Either[CompilationError, Unit] = {
      l match {
        case (v: VerifierAnnotation) :: Nil                           => Right(())
        case (c: CallableAnnotation) :: Nil                           => Right(())
        case _                                                        => Left(Generic(0, 0, "Unsupported annotation set"))
      }
    }
  }
  case class CallableAnnotation(invocationArgName: String) extends Annotation {
    lazy val dic = Map(invocationArgName -> com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.invocationType.typeRef)
  }
  case class VerifierAnnotation(txArgName: String) extends Annotation { lazy val dic = Map(txArgName -> WavesContext.verifierInput.typeRef) }

  sealed trait AnnotatedFunction {
    def annotation: Annotation
    def u: Terms.FUNC
  }
  case class ContractFunction(override val annotation: CallableAnnotation, override val u: Terms.FUNC) extends AnnotatedFunction
  case class VerifierFunction(override val annotation: VerifierAnnotation, override val u: Terms.FUNC) extends AnnotatedFunction
}
