package com.wavesplatform.lang.contract
import com.wavesplatform.lang.contract.Contract.{ContractFunction, VerifierFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.UserFunction
import com.wavesplatform.lang.v1.parser.Expressions.Declaration

/*
 Contact is a list of annotated definitions
 ContractInvokation is (ContractFunction name, ContractFunction args)

 In PoC:
  - No declarations
  - ContractFunctions can't invoke each other
 */


case class Contract(
    dec: List[Declaration],
    cfs: List[ContractFunction],
    vf: Option[VerifierFunction]
)

object Contract {

  sealed trait Annotation
  object Annotation {
    def parse(name: String, args: List[String]): Either[String, Annotation] = {
      (name, args) match {
        case ("Callable", s :: Nil)           => Right(CallableAnnotation(s))
        case ("Verifier", s :: Nil)           => Right(VerifierAnnotation(s))
        case ("Payable", amt :: token :: Nil) => Right(PayableAnnotation(amt, token))
        case _                                => Left("Annotation not recognized")
      }
    }
  }
  case class CallableAnnotation(pubKeyArgName: String)                      extends Annotation
  case class PayableAnnotation(amountArgName: String, tokenArgName: String) extends Annotation
  case class VerifierAnnotation(txArgName: String)                          extends Annotation

  case class ContractFunction(c: CallableAnnotation, p: Option[PayableAnnotation], u: UserFunction)
  case class VerifierFunction(v: VerifierAnnotation, u: UserFunction)
}
