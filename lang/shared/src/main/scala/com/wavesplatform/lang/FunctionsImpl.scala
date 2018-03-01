package com.wavesplatform.lang

import com.wavesplatform.lang.Terms.{BOOLEAN, BYTEVECTOR}
import com.wavesplatform.lang.ctx.PredefFunction
import com.wavesplatform.lang.traits.Crypto
import scodec.bits.ByteVector

abstract class FunctionsImpl { this: Crypto =>
  private def hashFunction(name: String)(h: Array[Byte] => Array[Byte]) = PredefFunction(name, BYTEVECTOR, List(("bytes", BYTEVECTOR))) {
    case (m: ByteVector) :: Nil => Right(ByteVector(h(m.toArray)))
    case _                      => ???
  }

  val keccack256F: PredefFunction = hashFunction("keccack256")(this.keccack256)
  val blake2b256F: PredefFunction = hashFunction("blake2b256")(this.blake2b256)
  val sha256F: PredefFunction     = hashFunction("sha256")(this.sha256)

  val sigVerifyF: PredefFunction = PredefFunction("SIGVERIFY", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
    case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
      Right(this.curve25519verify(s.toArray, m.toArray, p.toArray))
    case _ => ???
  }
}
