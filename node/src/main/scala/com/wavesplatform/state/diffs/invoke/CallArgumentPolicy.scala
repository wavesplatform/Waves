package com.wavesplatform.state.diffs.invoke

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.compiler.Terms._

trait CallArgumentPolicy {
  def check(e: EXPR): Boolean
  val expectedTypes: Set[String]
}

object CallArgumentPolicy {
  case object OnlyPrimitives extends CallArgumentPolicy {
    override def check(e: EXPR): Boolean =
      e match {
        case _: CONST_LONG | _: CONST_BYTESTR | _: CONST_STRING | _: CONST_BOOLEAN => true
        case _                                                                     => false
      }
    override val expectedTypes: Set[String] = ContractCompiler.primitiveCallableTypes
  }

  case object PrimitivesAndLists extends CallArgumentPolicy {
    override def check(e: EXPR): Boolean    = OnlyPrimitives.check(e) || e.isInstanceOf[ARR]
    override val expectedTypes: Set[String] = ContractCompiler.allowedCallableTypesV4
  }

  implicit class CallCheck(fc: FUNCTION_CALL) {
    def check(c: CallArgumentPolicy): Either[ExecutionError, Unit] =
      Either.cond(
        fc.args.forall(c.check),
        (),
        s"All arguments of InvokeScript must be one of the types: ${c.expectedTypes.mkString(", ")}"
      )
  }
}
