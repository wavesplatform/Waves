package com.wavesplatform.lang.v1.evaluator.ctx.impl

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_STRING}
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, STRING}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, NativeFunction}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}
import com.wavesplatform.lang.directives.values._

object CryptoContext {

  def build(global: BaseGlobal, version: StdLibVersion): CTX = {
    def hashFunction(name: String, internalName: Short, cost: Long, docString: String)(h: Array[Byte] => Array[Byte]): BaseFunction =
      NativeFunction(name, cost, internalName, BYTESTR, docString, ("bytes", BYTESTR, "value")) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => Right(CONST_BYTESTR(ByteStr(h(m.arr))))
        case _                                      => ???
      }

    val keccak256F: BaseFunction  = hashFunction("keccak256", KECCAK256, 10, "256 bit Keccak/SHA-3/TIPS-202")(global.keccak256)
    val blake2b256F: BaseFunction = hashFunction("blake2b256", BLAKE256, 10, "256 bit BLAKE")(global.blake2b256)
    val sha256F: BaseFunction     = hashFunction("sha256", SHA256, 10, "256 bit SHA-2")(global.sha256)

    val sigVerifyF: BaseFunction =
      NativeFunction("sigVerify",
                     100,
                     SIGVERIFY,
                     BOOLEAN,
                     "check signature",
                     ("message", BYTESTR, "value"),
                     ("sig", BYTESTR, "signature"),
                     ("pub", BYTESTR, "public key")) {
        case CONST_BYTESTR(m: ByteStr) :: CONST_BYTESTR(s: ByteStr) :: CONST_BYTESTR(p: ByteStr) :: Nil =>
          Right(CONST_BOOLEAN(global.curve25519verify(m.arr, s.arr, p.arr)))
        case _ => ???
      }

    def toBase58StringF: BaseFunction = NativeFunction("toBase58String", 10, TOBASE58, STRING, "Base58 encode", ("bytes", BYTESTR, "value")) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base58Encode(bytes.arr).map(CONST_STRING)
      case xs                                         => notImplemented("toBase58String(bytes: byte[])", xs)
    }

    def fromBase58StringF: BaseFunction =
      NativeFunction("fromBase58String", 10, FROMBASE58, BYTESTR, "Base58 decode", ("str", STRING, "base58 encoded string")) {
        case CONST_STRING(str: String) :: Nil => global.base58Decode(str, global.MaxBase58String).map(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented("fromBase58String(str: String)", xs)
      }

    def toBase64StringF: BaseFunction = NativeFunction("toBase64String", 10, TOBASE64, STRING, "Base64 encode", ("bytes", BYTESTR, "value")) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base64Encode(bytes.arr).map(CONST_STRING)
      case xs                                         => notImplemented("toBase64String(bytes: byte[])", xs)
    }

    def fromBase64StringF: BaseFunction =
      NativeFunction("fromBase64String", 10, FROMBASE64, BYTESTR, "Base64 decode", ("str", STRING, "base64 encoded string")) {
        case CONST_STRING(str: String) :: Nil => global.base64Decode(str, global.MaxBase64String).map(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented("fromBase64String(str: String)", xs)
      }

    def toBase16StringF: BaseFunction = NativeFunction("toBase16String", 10, TOBASE16, STRING, "Base16 encode", ("bytes", BYTESTR, "value")) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base16Encode(bytes.arr).map(CONST_STRING)
      case xs                                         => notImplemented("toBase16String(bytes: byte[])", xs)
    }

    def fromBase16StringF: BaseFunction =
      NativeFunction("fromBase16String", 10, FROMBASE16, BYTESTR, "Base16 decode", ("str", STRING, "base16 encoded string")) {
        case CONST_STRING(str: String) :: Nil => global.base16Decode(str, global.MaxBase64String).map(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented("fromBase16String(str: String)", xs)
      }
 
    CTX(
      Seq.empty,
      Map.empty,
      Array(keccak256F, blake2b256F, sha256F, sigVerifyF, toBase58StringF, fromBase58StringF, toBase64StringF, fromBase64StringF) ++
          (version match {
            case V1 | V2 => Array[BaseFunction]()
            case V3 => Array(toBase16StringF, fromBase16StringF)
          })
    )
  }

  def evalContext(global: BaseGlobal, version: StdLibVersion): EvaluationContext   = build(global, version).evaluationContext
  def compilerContext(global: BaseGlobal, version: StdLibVersion): CompilerContext = build(global, version).compilerContext
}
