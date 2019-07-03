package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.Eval
import cats.data.EitherT
import cats.syntax.either._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, _}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_STRING, CaseObj}
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, CASETYPEREF, FINAL, STRING, UNION}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, LazyVal, NativeFunction}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

object CryptoContext {

  private val none    = CASETYPEREF("NoAlg", List.empty)
  private val md5     = CASETYPEREF("Md5", List.empty)
  private val sha1    = CASETYPEREF("Sha1", List.empty)
  private val sha224  = CASETYPEREF("Sha224", List.empty)
  private val sha256  = CASETYPEREF("Sha256", List.empty)
  private val sha384  = CASETYPEREF("Sha384", List.empty)
  private val sha512  = CASETYPEREF("Sha512", List.empty)
  private val sha3224 = CASETYPEREF("Sha3224", List.empty)
  private val sha3256 = CASETYPEREF("Sha3256", List.empty)
  private val sha3384 = CASETYPEREF("Sha3384", List.empty)
  private val sha3512 = CASETYPEREF("Sha3512", List.empty)

  private val digestAlgorithmType =
    UNION(none, md5, sha1, sha224, sha256, sha384, sha512, sha3224, sha3256, sha3384, sha3512)

  private def algFromCO(obj: Terms.CaseObj): Either[String, DigestAlgorithm] = {
    import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA._
    obj match {
      case CaseObj(`none`, _)    => Right(NONE)
      case CaseObj(`md5`, _)     => Right(MD5)
      case CaseObj(`sha1`, _)    => Right(SHA1)
      case CaseObj(`sha224`, _)  => Right(SHA224)
      case CaseObj(`sha256`, _)  => Right(SHA256)
      case CaseObj(`sha384`, _)  => Right(SHA384)
      case CaseObj(`sha512`, _)  => Right(SHA512)
      case CaseObj(`sha3224`, _) => Right(SHA3224)
      case CaseObj(`sha3256`, _) => Right(SHA3256)
      case CaseObj(`sha3384`, _) => Right(SHA3384)
      case CaseObj(`sha3512`, _) => Right(SHA3512)
      case _                     => Left("Unknown digest type")
    }
  }

  private def digestAlgValue(tpe: CASETYPEREF): LazyVal = LazyVal(EitherT(Eval.always(CaseObj(tpe, Map.empty).asRight[String])))

  def build(global: BaseGlobal, version: StdLibVersion): CTX = {
    def hashFunction(name: String, internalName: Short, cost: Long, docString: String)(h: Array[Byte] => Array[Byte]): BaseFunction =
      NativeFunction(name, cost, internalName, BYTESTR, docString, ("bytes", BYTESTR, "value")) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case _                                => ???
      }

    val keccak256F: BaseFunction  = hashFunction("keccak256", KECCAK256, 10, "256 bit Keccak/SHA-3/TIPS-202")(global.keccak256)
    val blake2b256F: BaseFunction = hashFunction("blake2b256", BLAKE256, 10, "256 bit BLAKE")(global.blake2b256)
    val sha256F: BaseFunction     = hashFunction("sha256", SHA256, 10, "256 bit SHA-2")(global.sha256)

    def sigVerifyF(contextVer: StdLibVersion): BaseFunction =
      NativeFunction("sigVerify",
                     100,
                     SIGVERIFY,
                     BOOLEAN,
                     "check signature",
                     ("message", BYTESTR, "value"),
                     ("sig", BYTESTR, "signature"),
                     ("pub", BYTESTR, "public key")) {
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if (contextVer != V1 && contextVer != V2 && msg.size > global.MaxByteStrSizeForVerifyFuncs) =>
          Left(s"Invalid message size, must be not greater than ${global.MaxByteStrSizeForVerifyFuncs / 1024} KB")
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          Right(CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)))
        case _ => ???
      }

    val rsaVerifyF: BaseFunction =
      NativeFunction(
        "rsaVerify",
        300,
        RSAVERIFY,
        BOOLEAN,
        "check RSA signature",
        ("digest", digestAlgorithmType, "digest algorithm"),
        ("message", BYTESTR, "value"),
        ("sig", BYTESTR, "signature"),
        ("pub", BYTESTR, "public key")
      ) {
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if (msg.size > global.MaxByteStrSizeForVerifyFuncs) =>
          Left(s"Invalid message size, must be not greater than ${global.MaxByteStrSizeForVerifyFuncs / 1024} KB")
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          algFromCO(digestAlg) map { alg =>
            CONST_BOOLEAN(global.rsaVerify(alg, msg.arr, sig.arr, pub.arr))
          }
        case _ => ???
      }

    def toBase58StringF: BaseFunction = NativeFunction("toBase58String", 10, TOBASE58, STRING, "Base58 encode", ("bytes", BYTESTR, "value")) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base58Encode(bytes.arr).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented("toBase58String(bytes: ByteVector)", xs)
    }

    def fromBase58StringF: BaseFunction =
      NativeFunction("fromBase58String", 10, FROMBASE58, BYTESTR, "Base58 decode", ("str", STRING, "base58 encoded string")) {
        case CONST_STRING(str: String) :: Nil => global.base58Decode(str, global.MaxBase58String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented("fromBase58String(str: String)", xs)
      }

    def toBase64StringF: BaseFunction = NativeFunction("toBase64String", 10, TOBASE64, STRING, "Base64 encode", ("bytes", BYTESTR, "value")) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base64Encode(bytes.arr).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented("toBase64String(bytes: ByteVector)", xs)
    }

    def fromBase64StringF: BaseFunction =
      NativeFunction("fromBase64String", 10, FROMBASE64, BYTESTR, "Base64 decode", ("str", STRING, "base64 encoded string")) {
        case CONST_STRING(str: String) :: Nil => global.base64Decode(str, global.MaxBase64String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented("fromBase64String(str: String)", xs)
      }

    val checkMerkleProofF: BaseFunction =
      NativeFunction(
        "checkMerkleProof",
        30,
        CHECK_MERKLE_PROOF,
        BOOLEAN,
        "Check validity of Merkle tree proof",
        ("merkleRoot", BYTESTR, "root hash of Merkle tree"),
        ("merkleProof", BYTESTR, "proof bytes"),
        ("valueBytes", BYTESTR, "bytes of value that must be proven")
      ) {
        case CONST_BYTESTR(root) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(value) :: Nil =>
          Right(CONST_BOOLEAN(global.merkleVerify(root, proof, value)))
        case _ => ???
      }

    def toBase16StringF: BaseFunction = NativeFunction("toBase16String", 10, TOBASE16, STRING, "Base16 encode", ("bytes", BYTESTR, "value")) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base16Encode(bytes.arr).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented("toBase16String(bytes: ByteVector)", xs)
    }

    def fromBase16StringF: BaseFunction =
      NativeFunction("fromBase16String", 10, FROMBASE16, BYTESTR, "Base16 decode", ("str", STRING, "base16 encoded string")) {
        case CONST_STRING(str: String) :: Nil => global.base16Decode(str, global.MaxBase64String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented("fromBase16String(str: String)", xs)
      }

    val v1Functions =
      Array(
        keccak256F,
        blake2b256F,
        sha256F,
        sigVerifyF(version),
        toBase58StringF,
        fromBase58StringF,
        toBase64StringF,
        fromBase64StringF
      )

    val v3Types = List(
      none,
      md5,
      sha1,
      sha224,
      sha256,
      sha384,
      sha512,
      sha3224,
      sha3256,
      sha3384,
      sha3512,
      digestAlgorithmType
    )

    val v3Vars: Map[String, ((FINAL, String), LazyVal)] = Map(
      ("NOALG", ((none, "NONE digest algorithm"), digestAlgValue(none))),
      ("MD5", ((md5, "MD5 digest algorithm"), digestAlgValue(md5))),
      ("SHA1", ((sha1, "SHA1 digest algorithm"), digestAlgValue(sha1))),
      ("SHA224", ((sha224, "SHA224 digest algorithm"), digestAlgValue(sha224))),
      ("SHA256", ((sha256, "SHA256 digest algorithm"), digestAlgValue(sha256))),
      ("SHA384", ((sha384, "SHA384 digest algorithm"), digestAlgValue(sha384))),
      ("SHA512", ((sha512, "SHA512 digest algorithm"), digestAlgValue(sha512))),
      ("SHA3224", ((sha3224, "SHA3-224 digest algorithm"), digestAlgValue(sha3224))),
      ("SHA3256", ((sha3256, "SHA3-256 digest algorithm"), digestAlgValue(sha3256))),
      ("SHA3384", ((sha3384, "SHA3-384 digest algorithm"), digestAlgValue(sha3384))),
      ("SHA3512", ((sha3512, "SHA3-512 digest algorithm"), digestAlgValue(sha3512)))
    )

    val v3Functions =
      Array(
        rsaVerifyF,
        checkMerkleProofF,
        toBase16StringF,
        fromBase16StringF
      )

    version match {
      case V1 | V2 => CTX(Seq.empty, Map.empty, v1Functions)
      case V3      => CTX(v3Types, v3Vars, v1Functions ++ v3Functions)
    }
  }

  def evalContext(global: BaseGlobal, version: StdLibVersion): EvaluationContext   = build(global, version).evaluationContext
  def compilerContext(global: BaseGlobal, version: StdLibVersion): CompilerContext = build(global, version).compilerContext
}
