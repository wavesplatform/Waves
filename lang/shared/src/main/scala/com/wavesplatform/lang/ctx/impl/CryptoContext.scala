package com.wavesplatform.lang.ctx.impl

import com.wavesplatform.lang.BaseGlobal
import com.wavesplatform.lang.Terms.{BOOLEAN, BYTEVECTOR, STRING}
import com.wavesplatform.lang.ctx.{Context, PredefFunction}
import scodec.bits.ByteVector

object CryptoContext {

  def build(global: BaseGlobal): Context = {

    def hashFunction(name: String)(h: Array[Byte] => Array[Byte]) = PredefFunction(name, BYTEVECTOR, List(("bytes", BYTEVECTOR))) {
      case (m: ByteVector) :: Nil => Right(ByteVector(h(m.toArray)))
      case _                      => ???
    }

    val keccak256F: PredefFunction  = hashFunction("keccak256")(global.keccak256)
    val blake2b256F: PredefFunction = hashFunction("blake2b256")(global.blake2b256)
    val sha256F: PredefFunction     = hashFunction("sha256")(global.sha256)

    val sigVerifyF: PredefFunction = PredefFunction("sigVerify", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
      case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
        Right(global.curve25519verify(m.toArray, s.toArray, p.toArray))
      case _ => ???
    }

    def toBase58StringF: PredefFunction = PredefFunction("toBase58String", STRING, List(("bytes", BYTEVECTOR))) {
      case (bytes: ByteVector) :: Nil =>
        Right(global.base58Encode(bytes.toArray))
      case _ => ???
    }
    Context.build(Seq.empty, Map.empty, Seq(keccak256F, blake2b256F, sha256F, sigVerifyF, toBase58StringF))
  }
}
