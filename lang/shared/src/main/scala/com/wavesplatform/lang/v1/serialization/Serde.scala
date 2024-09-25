package com.wavesplatform.lang.v1.serialization

import com.wavesplatform.lang.v1.compiler.Terms.*
import monix.eval.Coeval

trait Serde[In, Out] {
  def serializeDeclaration(out: Out, dec: DECLARATION, aux: EXPR => Coeval[Unit]): Coeval[Unit]
  def deserializeDeclaration(in: In, aux: => Coeval[EXPR], decType: Byte): Coeval[DECLARATION]
  def desAux(in: In, allowObjects: Boolean = false, acc: Coeval[Unit] = Coeval.now(())): Coeval[EXPR]
  def deserialize(bytes: Array[Byte], all: Boolean = true, allowObjects: Boolean = false): Either[String, (EXPR, Int)]
  def deserialize(in: In): Either[String, EXPR]
  def deserializeFunctionCall(in: In): Either[Throwable, FUNCTION_CALL]
  def deserializeFunctionCall(array: Array[Byte]): Either[Throwable, FUNCTION_CALL]
  def serAux(out: Out, acc: Coeval[Unit], expr: EXPR, allowObjects: Boolean = false): Coeval[Unit]
  def serialize(expr: EXPR, allowObjects: Boolean = false): Array[Byte]

  protected def evaluatedOnly(arg: Coeval[EXPR]): Coeval[EVALUATED] =
    arg.flatMap {
      case value: EVALUATED => Coeval.now(value)
      case other            => Coeval.raiseError(new Exception(s"Unsupported array element: $other"))
    }
}

object Serde {
  val E_LONG: Byte    = 0
  val E_BYTES: Byte   = 1
  val E_STRING: Byte  = 2
  val E_IF: Byte      = 3
  val E_BLOCK: Byte   = 4
  val E_REF: Byte     = 5
  val E_TRUE: Byte    = 6
  val E_FALSE: Byte   = 7
  val E_GETTER: Byte  = 8
  val E_FUNCALL: Byte = 9

  val FH_NATIVE: Byte = 0
  val FH_USER: Byte   = 1

  val E_BLOCK_V2: Byte = 10
  val E_ARR: Byte      = 11
  val E_CASE_OBJ: Byte = 12

  val DEC_LET: Byte  = 0
  val DEC_FUNC: Byte = 1
}
