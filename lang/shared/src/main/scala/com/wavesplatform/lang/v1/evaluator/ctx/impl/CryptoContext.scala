package com.wavesplatform.lang.v1.evaluator.ctx.impl

import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTEVECTOR, STRING}
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, PredefFunction}
import scodec.bits.ByteVector
import com.wavesplatform.lang.v1.evaluator.FunctionIds._

object CryptoContext {

  def build(global: BaseGlobal): EvaluationContext = {

    def hashFunction(name: String, internalName: Short, cost: Long)(h: Array[Byte] => Array[Byte]) = PredefFunction(name, cost, BYTEVECTOR, List(("bytes", BYTEVECTOR)), internalName) {
      case (m: ByteVector) :: Nil => Right(ByteVector(h(m.toArray)))
      case _                      => ???
    }

    val keccak256F: PredefFunction  = hashFunction("keccak256", KECCAK256, 10)(global.keccak256)
    val blake2b256F: PredefFunction = hashFunction("blake2b256", BLAKE256, 10)(global.blake2b256)
    val sha256F: PredefFunction     = hashFunction("sha256", SHA256, 10)(global.sha256)

    val sigVerifyF: PredefFunction =
      PredefFunction("sigVerify", 100, BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR)), SIGVERIFY) {
        case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
          Right(global.curve25519verify(m.toArray, s.toArray, p.toArray))
        case _ => ???
      }

    def toBase58StringF: PredefFunction = PredefFunction("toBase58String", 10, STRING, List(("bytes", BYTEVECTOR)), TOBASE58) {
      case (bytes: ByteVector) :: Nil => global.base58Encode(bytes.toArray)
      case _ => ???
    }

    def toBase64StringF: PredefFunction = PredefFunction("toBase64String", 10, STRING, List(("bytes", BYTEVECTOR)), TOBASE64) {
      case (bytes: ByteVector) :: Nil => global.base64Encode(bytes.toArray)
      case _ => ???
    }
    EvaluationContext.build(Map.empty, Seq(keccak256F, blake2b256F, sha256F, sigVerifyF, toBase58StringF, toBase64StringF))
  }
}
