package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.implicits._
import cats.{Id, Monad}
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.merkle.Merkle.createRoot
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, _}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, NativeFunction}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}
import com.wavesplatform.lang.ExecutionError

import scala.util.Try

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

  private def digestAlgValue(tpe: CASETYPEREF): ContextfulVal[NoContext] =
    ContextfulVal.pure(CaseObj(tpe, Map.empty))

  def build(global: BaseGlobal, version: StdLibVersion): CTX[NoContext] = {
    def lgen(lim: Array[Int], name: ((Int,Int)) => (String, Short), complexity: Int => Int, check: Int => List[EVALUATED] => Either[ExecutionError, Unit], ret: TYPE, args: (String, TYPE)*)(body: List[EVALUATED] => Either[ExecutionError, EVALUATED]) : Array[BaseFunction[NoContext]] = {
      lim.zipWithIndex.map { n =>
        val (sname,iname) = name(n)
        NativeFunction[NoContext](sname,
                       complexity(n._1),
                       iname,
                       ret,
                       args : _*) { a =>
                          check(n._1)(a).flatMap(_ => body(a))
                       }
      }
    }

    def hashFunction(name: String, internalName: Short, cost: Long)(h: Array[Byte] => Array[Byte]): BaseFunction[NoContext] =
      NativeFunction(name, cost, internalName, BYTESTR, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case xs                               => notImplemented[Id, EVALUATED](s"$name(bytes: ByteVector)", xs)
      }

    val keccak256F: BaseFunction[NoContext]  = hashFunction("keccak256", KECCAK256, (if(version < V4) { 10 } else { 200 }))(global.keccak256)
    val blake2b256F: BaseFunction[NoContext] = hashFunction("blake2b256", BLAKE256, (if(version < V4) { 10 } else { 200 }))(global.blake2b256)
    val sha256F: BaseFunction[NoContext]     = hashFunction("sha256", SHA256, (if(version < V4) { 10 } else { 200 }))(global.sha256)

    def hashLimFunction(lim: Array[Int], name: String, internalName: Short, costs: Int => Int)(h: Array[Byte] => Array[Byte]): Array[BaseFunction[NoContext]] =
      lgen(lim, (n => (s"${name}_${n._1}Kb", (internalName + n._2).toShort)), costs, (n => {
                                                            case CONST_BYTESTR(msg: ByteStr) :: _ => Either.cond(msg.size <= n*1024, (), s"Invalid message size, must be not greater than $n Kb")
                                                            case xs => notImplemented[Id, Unit](s"${name}_${n}Kb(bytes: ByteVector)", xs)
                                                          }), BYTESTR, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case xs                               => notImplemented[Id, EVALUATED](s"${name}_NKb(bytes: ByteVector)", xs)
      }

    val keccak256F_lim: Array[BaseFunction[NoContext]]  = hashLimFunction(Array(16,32,64,128), "keccak256", KECCAK256_LIM, ({ case 16 => 10
                                                                                                                              case 32 => 25
                                                                                                                              case 64 => 50
                                                                                                                              case 128 => 100 }))(global.keccak256)
    val blake2b256F_lim: Array[BaseFunction[NoContext]]  = hashLimFunction(Array(16,32,64,128), "blake2b256", BLAKE256_LIM, ({ case 16 => 10
                                                                                                                               case 32 => 25
                                                                                                                               case 64 => 50
                                                                                                                               case 128 => 100 }))(global.blake2b256)
    val sha256F_lim: Array[BaseFunction[NoContext]]  = hashLimFunction(Array(16,32,64,128), "sha256", SHA256_LIM, ({ case 16 => 10
                                                                                                                     case 32 => 25
                                                                                                                     case 64 => 50
                                                                                                                     case 128 => 100 }))(global.sha256)

    val sigVerifyL: Array[BaseFunction[NoContext]] = lgen(Array(16,32,64,128),
                                                         (n => (s"sigVerify_${n._1}Kb", (SIGVERIFY_LIM + n._2).toShort)),
                                                         ({ case 16 => 100
                                                            case 32 => 110
                                                            case 64 => 125
                                                            case 128 => 150
                                                         }),
                                                         (n => {
                                                            case CONST_BYTESTR(msg: ByteStr) :: _ => Either.cond(msg.size <= n*1024, (), s"Invalid message size, must be not greater than $n Kb")
                                                            case xs => notImplemented[Id, Unit](s"sigVerify_${n}Kb(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
                                                          }),
                                                        BOOLEAN,
                                                        ("message", BYTESTR),
                                                        ("sig", BYTESTR),
                                                        ("pub", BYTESTR)) {
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          Right(CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)))
        case xs => notImplemented[Id, EVALUATED](s"sigVerify(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
    }


    def sigVerifyF(contextVer: StdLibVersion): BaseFunction[NoContext] = {
      val lim = if(version < V4) { global.MaxByteStrSizeForVerifyFuncs } else { global.MaxByteStrSizeForVerifyFuncs_V4 }
      NativeFunction("sigVerify",
                     (if(version < V4) { 100 } else { 200 }),
                     SIGVERIFY,
                     BOOLEAN,
                     ("message", BYTESTR),
                     ("sig", BYTESTR),
                     ("pub", BYTESTR)) {
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if (contextVer != V1 && contextVer != V2 && msg.size > lim) =>
          Left(s"Invalid message size, must be not greater than ${lim / 1024} KB")
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          Right(CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)))
        case xs => notImplemented[Id, EVALUATED](s"sigVerify(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    val rsaVerifyF: BaseFunction[NoContext] = {
      val lim = if(version < V4) { global.MaxByteStrSizeForVerifyFuncs } else { global.MaxByteStrSizeForVerifyFuncs_V4 }
      NativeFunction(
        "rsaVerify",
        (if(version < V4) { 300 } else { 1000 }),
        RSAVERIFY,
        BOOLEAN,
        ("digest", digestAlgorithmType),
        ("message", BYTESTR),
        ("sig", BYTESTR),
        ("pub", BYTESTR)
      ) {
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if (msg.size > lim) =>
          Left(s"Invalid message size, must be not greater than ${lim / 1024} KB")
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          algFromCO(digestAlg) flatMap { alg =>
            Try(global.rsaVerify(alg, msg.arr, sig.arr, pub.arr))
              .toEither
              .bimap(_ => "Illegal input params", CONST_BOOLEAN)
          }
        case xs => notImplemented[Id, EVALUATED](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    val rsaVerifyL: Array[BaseFunction[NoContext]] = lgen(Array(16,32,64,128),
                                                         (n => (s"rsaVerify_${n._1}Kb", (RSAVERIFY_LIM + n._2).toShort)),
                                                         ({ case 16 => 500
                                                            case 32 => 550
                                                            case 64 => 625
                                                            case 128 => 750
                                                         }),
                                                         (n => {
                                                            case _ :: CONST_BYTESTR(msg: ByteStr) :: _ => Either.cond(msg.size <= n*1024, (), s"Invalid message size, must be not greater than $n Kb")
                                                            case xs => notImplemented[Id, Unit](s"rsaVerify_${n}Kb(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
                                                          }),
                                                        BOOLEAN,
                                                        ("digest", digestAlgorithmType),
                                                        ("message", BYTESTR),
                                                        ("sig", BYTESTR),
                                                        ("pub", BYTESTR)) {
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          algFromCO(digestAlg) flatMap { alg =>
            Try(global.rsaVerify(alg, msg.arr, sig.arr, pub.arr))
              .toEither
              .bimap(_ => "Illegal input params", CONST_BOOLEAN)
          }
        case xs => notImplemented[Id, EVALUATED](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }

    def toBase58StringF: BaseFunction[NoContext] = NativeFunction("toBase58String", 10, TOBASE58, STRING, ("bytes", BYTESTR)) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base58Encode(bytes.arr).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented[Id, EVALUATED]("toBase58String(bytes: ByteVector)", xs)
    }

    def fromBase58StringF: BaseFunction[NoContext] =
      NativeFunction("fromBase58String", 10, FROMBASE58, BYTESTR, ("str", STRING)) {
        case CONST_STRING(str: String) :: Nil => global.base58Decode(str, global.MaxBase58String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id, EVALUATED]("fromBase58String(str: String)", xs)
      }

    def toBase64StringF: BaseFunction[NoContext] = NativeFunction("toBase64String", 10, TOBASE64, STRING, ("bytes", BYTESTR)) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base64Encode(bytes.arr).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented[Id, EVALUATED]("toBase64String(bytes: ByteVector)", xs)
    }

    def fromBase64StringF: BaseFunction[NoContext] =
      NativeFunction("fromBase64String", 10, FROMBASE64, BYTESTR, ("str", STRING)) {
        case CONST_STRING(str: String) :: Nil => global.base64Decode(str, global.MaxBase64String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id, EVALUATED]("fromBase64String(str: String)", xs)
      }

    val checkMerkleProofF: BaseFunction[NoContext] =
      NativeFunction(
        "checkMerkleProof",
        30,
        CHECK_MERKLE_PROOF,
        BOOLEAN,
        ("merkleRoot", BYTESTR),
        ("merkleProof", BYTESTR),
        ("valueBytes", BYTESTR)
      ) {
        case CONST_BYTESTR(root) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(value) :: Nil =>
          Right(CONST_BOOLEAN(global.merkleVerify(root, proof, value)))
        case xs => notImplemented[Id, EVALUATED](s"checkMerkleProof(merkleRoot: ByteVector, merkleProof: ByteVector, valueBytes: ByteVector)", xs)
      }

  val createMerkleRootF: BaseFunction[NoContext] =
    NativeFunction(
      "createMerkleRoot",
      30,
      CREATE_MERKLE_ROOT,
      BYTESTR,
      ("merkleProof", LIST(BYTESTR)),
      ("valueBytes", BYTESTR),
      ("index", LONG)
    ) {
        case xs@(ARR(proof) :: CONST_BYTESTR(value) :: CONST_LONG(index) :: Nil) =>
          if(value.size == 32 && proof.length <= 16 && proof.forall({
              case CONST_BYTESTR(v) => v.size == 32
              case _ => false
            })) {
            CONST_BYTESTR(createRoot(value, Math.toIntExact(index), proof.map({
               case CONST_BYTESTR(v) => v.arr
               case _ => throw(new Exception("Expect ByteStr"))
            }))) .left.map(_.toString)
          } else {
            notImplemented[Id, EVALUATED](s"createMerkleRoot(merkleProof: ByteVector, valueBytes: ByteVector)", xs)
          }
        case xs => notImplemented[Id, EVALUATED](s"createMerkleRoot(merkleProof: ByteVector, valueBytes: ByteVector)", xs)
    }

    def toBase16StringF(checkLength: Boolean): BaseFunction[NoContext] = NativeFunction("toBase16String", 10, TOBASE16, STRING, ("bytes", BYTESTR)) {
      case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base16Encode(bytes.arr, checkLength).flatMap(CONST_STRING(_))
      case xs                                   => notImplemented[Id, EVALUATED]("toBase16String(bytes: ByteVector)", xs)
    }

    def fromBase16StringF(checkLength: Boolean): BaseFunction[NoContext] =
      NativeFunction("fromBase16String", 10, FROMBASE16, BYTESTR, ("str", STRING)) {
        case CONST_STRING(str: String) :: Nil => global.base16Decode(str, checkLength).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id, EVALUATED]("fromBase16String(str: String)", xs)
      }

    val bls12Groth16VerifyL: Array[BaseFunction[NoContext]] = lgen((1 to 15).toArray,
                                                         (n => (s"groth16Verify_${n._1}inputs", (BLS12_GROTH16_VERIFY_LIM + n._2).toShort)),
                                                         ({ n =>
                                                           Array(1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600)(n-1)
                                                         }),
                                                         (n => {
                                                            case _ :: _ :: CONST_BYTESTR(inputs: ByteStr) :: _ => Either.cond(inputs.size <= n*32, (), s"Invalid inputs size ${inputs.size} bytes, must be not greater than ${n*32} bytes")
                                                            case xs => notImplemented[Id, Unit](s"groth16Verify_${n}inputs(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
                                                          }),
                                                        BOOLEAN,
                                                        ("verifying key", BYTESTR),
                                                        ("proof", BYTESTR),
                                                        ("inputs", BYTESTR)) {
        case CONST_BYTESTR(vk:ByteStr) :: CONST_BYTESTR(proof:ByteStr) :: CONST_BYTESTR(inputs:ByteStr) :: Nil =>
          if (inputs.size > 512)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than 512 bytes")
          else if (inputs.size % 32 != 0)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
          else if (proof.size != 192)
            Left(s"Invalid proof size ${proof.size} bytes, must be equal to 192 bytes")
          else if (vk.size != 48 * (8 + inputs.size / 32))
            Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${(8 + inputs.size/32)*48} bytes for ${inputs.size/32} inputs")
          else
            Right(CONST_BOOLEAN(global.groth16Verify(vk.arr, proof.arr, inputs.arr)))
        case xs => notImplemented[Id, EVALUATED]("groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      }


    val bls12Groth16VerifyF: BaseFunction[NoContext] =
      NativeFunction(
        "groth16Verify",
        2700,
        BLS12_GROTH16_VERIFY,
        BOOLEAN,
        ("verifying key", BYTESTR),
        ("proof", BYTESTR),
        ("inputs", BYTESTR)
      ) {
        case CONST_BYTESTR(vk:ByteStr) :: CONST_BYTESTR(proof:ByteStr) :: CONST_BYTESTR(inputs:ByteStr) :: Nil =>
          if (inputs.size > 512)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than 512 bytes")
          else if (inputs.size % 32 != 0)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
          else if (proof.size != 192)
            Left(s"Invalid proof size ${proof.size} bytes, must be equal to 192 bytes")
          else if (vk.size != 48 * (8 + inputs.size / 32))
            Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${(8 + inputs.size/32)*48} bytes for ${inputs.size/32} inputs")
          else
            Right(CONST_BOOLEAN(global.groth16Verify(vk.arr, proof.arr, inputs.arr)))
        case xs => notImplemented[Id, EVALUATED]("groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
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

    val v3Vars: Map[String, (FINAL, ContextfulVal[NoContext])] =
      Map(
        ("NOALG",   (none,    digestAlgValue(none))),
        ("MD5",     (md5,     digestAlgValue(md5))),
        ("SHA1",    (sha1,    digestAlgValue(sha1))),
        ("SHA224",  (sha224,  digestAlgValue(sha224))),
        ("SHA256",  (sha256,  digestAlgValue(sha256))),
        ("SHA384",  (sha384,  digestAlgValue(sha384))),
        ("SHA512",  (sha512,  digestAlgValue(sha512))),
        ("SHA3224", (sha3224, digestAlgValue(sha3224))),
        ("SHA3256", (sha3256, digestAlgValue(sha3256))),
        ("SHA3384", (sha3384, digestAlgValue(sha3384))),
        ("SHA3512", (sha3512, digestAlgValue(sha3512)))
      )

    val v3Functions =
      Array(
        rsaVerifyF,
        checkMerkleProofF,
        toBase16StringF(checkLength = false),
        fromBase16StringF(checkLength = false)
      )

    val v4Functions =
      Array(
        bls12Groth16VerifyF, createMerkleRootF,  // new in V4
        rsaVerifyF, toBase16StringF(checkLength = true), fromBase16StringF(checkLength = true) // from V3
        ) ++ sigVerifyL ++ rsaVerifyL ++ keccak256F_lim ++ blake2b256F_lim ++ sha256F_lim ++ bls12Groth16VerifyL

    val fromV1Ctx = CTX[NoContext](Seq(), Map(), v1Functions)
    val fromV3Ctx = fromV1Ctx |+| CTX[NoContext](v3Types, v3Vars, v3Functions)
    val fromV4Ctx = fromV1Ctx |+| CTX[NoContext](v3Types, v3Vars, v4Functions)

    version match {
      case V1 | V2      => fromV1Ctx
      case V3           => fromV3Ctx
      case v if v >= V4 => fromV4Ctx
    }
  }

  def evalContext[F[_]: Monad](global: BaseGlobal, version: StdLibVersion): EvaluationContext[NoContext, F] =
    build(global, version).evaluationContext[F]

  def compilerContext(global: BaseGlobal, version: StdLibVersion): CompilerContext =
    build(global, version).compilerContext
}
