package com.wavesplatform.lang.v1.ctx.impl

import com.wavesplatform.lang.v1.Terms.{BOOLEAN, BYTEVECTOR, STRING}
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.ctx.{Context, PredefFunction}
import scodec.bits.ByteVector

object CryptoContext {

  def build(global: BaseGlobal): Context = {

    def hashFunction(name: String, cost: Long)(h: Array[Byte] => Array[Byte]) = PredefFunction(name, cost, BYTEVECTOR, List(("bytes", BYTEVECTOR))) {
      case (m: ByteVector) :: Nil => Right(ByteVector(h(m.toArray)))
      case _                      => ???
    }

    val keccak256F: PredefFunction  = hashFunction("keccak256", 14000)(global.keccak256)
    val blake2b256F: PredefFunction = hashFunction("blake2b256", 3500)(global.blake2b256)
    val sha256F: PredefFunction     = hashFunction("sha256", 1000)(global.sha256)

    val sigVerifyF: PredefFunction =
      PredefFunction("sigVerify", 90000, BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
        case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
          Right(global.curve25519verify(m.toArray, s.toArray, p.toArray))
        case _ => ???
      }
    Context.build(Seq.empty, Map.empty, Seq(keccak256F, blake2b256F, sha256F, sigVerifyF))
  }
}
