package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.syntax.semigroup._
import cats.{Id, Monad}
import com.wavesplatform.common.merkle.Merkle.createRoot
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
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

import scala.collection.mutable

object CryptoContext {

  private val rsaTypeNames = List("NoAlg", "Md5", "Sha1", "Sha224", "Sha256", "Sha384", "Sha512", "Sha3224", "Sha3256", "Sha3384", "Sha3512")

  private def rsaHashAlgs(v: StdLibVersion) = {
    rsaTypeNames.map(CASETYPEREF(_, List.empty, v > V3))
  }

  private def digestAlgorithmType(v: StdLibVersion) =
    UNION.create(rsaHashAlgs(v), if (v > V3 && v < V6) Some("RsaDigestAlgs") else None)

  private val rsaHashLib = {
    import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA._
    rsaTypeNames.zip(List(NONE, MD5, SHA1, SHA224, SHA256, SHA384, SHA512, SHA3224, SHA3256, SHA3384, SHA3512)).toMap
  }

  private def algFromCO(obj: Terms.CaseObj): Either[String, DigestAlgorithm] = {
    rsaHashLib.get(obj.caseType.name).fold(Left[String, DigestAlgorithm]("Unknown digest type"): Either[String, DigestAlgorithm])(Right(_))
  }

  private def digestAlgValue(tpe: CASETYPEREF): ContextfulVal[NoContext] =
    ContextfulVal.pure(CaseObj(tpe, Map.empty))

  def build(global: BaseGlobal, version: StdLibVersion): CTX[NoContext] =
    ctxCache.getOrElse((global, version), ctxCache.synchronized {
      ctxCache.getOrElseUpdate((global, version), buildNew(global, version))
    })

  private val ctxCache = mutable.AnyRefMap.empty[(BaseGlobal, StdLibVersion), CTX[NoContext]]

  private def buildNew(global: BaseGlobal, version: StdLibVersion): CTX[NoContext] = {
    def lgen(
        lim: Array[Int],
        name: ((Int, Int)) => (String, Short),
        complexity: Int => Int,
        check: Int => List[EVALUATED] => Either[ExecutionError, Unit],
        ret: TYPE,
        args: (String, TYPE)*
    )(body: List[EVALUATED] => Either[ExecutionError, EVALUATED]): Array[BaseFunction[NoContext]] = {
      lim.zipWithIndex.map { n =>
        val (sname, iname) = name(n)
        NativeFunction[NoContext](sname, complexity(n._1), iname, ret, args: _*) { a =>
          check(n._1)(a).flatMap(_ => body(a))
        }
      }
    }

    def hashFunction(name: String, internalName: Short, cost: Long)(h: Array[Byte] => Array[Byte]): BaseFunction[NoContext] =
      NativeFunction(name, cost, internalName, BYTESTR, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case xs                               => notImplemented[Id, EVALUATED](s"$name(bytes: ByteVector)", xs)
      }

    val keccak256F: BaseFunction[NoContext] = hashFunction("keccak256", KECCAK256, (if (version < V4) {
                                                                                      10
                                                                                    } else {
                                                                                      200
                                                                                    }))(global.keccak256)
    val blake2b256F: BaseFunction[NoContext] = hashFunction("blake2b256", BLAKE256, (if (version < V4) {
                                                                                       10
                                                                                     } else {
                                                                                       200
                                                                                     }))(global.blake2b256)
    val sha256F: BaseFunction[NoContext] = hashFunction("sha256", SHA256, (if (version < V4) {
                                                                             10
                                                                           } else {
                                                                             200
                                                                           }))(global.sha256)

    def hashLimFunction(lim: Array[Int], name: String, internalName: Short, costs: Int => Int)(
        h: Array[Byte] => Array[Byte]
    ): Array[BaseFunction[NoContext]] =
      lgen(
        lim,
        (n => (s"${name}_${n._1}Kb", (internalName + n._2).toShort)),
        costs,
        (n => {
          case CONST_BYTESTR(msg: ByteStr) :: _ =>
            Either.cond(msg.size <= n * 1024, (), s"Invalid message size = ${msg.size} bytes, must be not greater than $n KB")
          case xs => notImplemented[Id, Unit](s"${name}_${n}Kb(bytes: ByteVector)", xs)
        }),
        BYTESTR,
        ("bytes", BYTESTR)
      ) {
        case CONST_BYTESTR(m: ByteStr) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case xs                               => notImplemented[Id, EVALUATED](s"${name}_NKb(bytes: ByteVector)", xs)
      }

    val keccak256F_lim: Array[BaseFunction[NoContext]] = hashLimFunction(Array(16, 32, 64, 128), "keccak256", KECCAK256_LIM, ({
      case 16  => 10
      case 32  => 25
      case 64  => 50
      case 128 => 100
    }))(global.keccak256)
    val blake2b256F_lim: Array[BaseFunction[NoContext]] = hashLimFunction(Array(16, 32, 64, 128), "blake2b256", BLAKE256_LIM, ({
      case 16  => 10
      case 32  => 25
      case 64  => 50
      case 128 => 100
    }))(global.blake2b256)
    val sha256F_lim: Array[BaseFunction[NoContext]] = hashLimFunction(Array(16, 32, 64, 128), "sha256", SHA256_LIM, ({
      case 16  => 10
      case 32  => 25
      case 64  => 50
      case 128 => 100
    }))(global.sha256)

    val sigVerifyL: Array[BaseFunction[NoContext]] = lgen(
      Array(8, 16, 32, 64, 128),
      (n => (s"sigVerify_${n._1}Kb", (SIGVERIFY_LIM + n._2).toShort)),
      ({
        case 8   => 47
        case 16  => 57
        case 32  => 70
        case 64  => 102
        case 128 => 172
      }),
      (n => {
        case CONST_BYTESTR(msg: ByteStr) :: _ =>
          Either.cond(msg.size <= n * 1024, (), s"Invalid message size = ${msg.size} bytes, must be not greater than $n KB")
        case xs => notImplemented[Id, Unit](s"sigVerify_${n}Kb(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }),
      BOOLEAN,
      ("message", BYTESTR),
      ("sig", BYTESTR),
      ("pub", BYTESTR)
    ) {
      case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
        Right(CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)))
      case xs => notImplemented[Id, EVALUATED](s"sigVerify(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
    }

    def sigVerifyF(contextVer: StdLibVersion): BaseFunction[NoContext] = {
      val lim = global.MaxByteStrSizeForVerifyFuncs
      NativeFunction("sigVerify", (if (version < V4) {
                                     100
                                   } else {
                                     200
                                   }), SIGVERIFY, BOOLEAN, ("message", BYTESTR), ("sig", BYTESTR), ("pub", BYTESTR)) {
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if (contextVer == V3 && msg.size > lim) =>
          Left(s"Invalid message size = ${msg.size} bytes, must be not greater than ${lim / 1024} KB")
        case CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          Right(CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)))
        case xs => notImplemented[Id, EVALUATED](s"sigVerify(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    def rsaVerify(
        digestAlg: CaseObj,
        msg: ByteStr,
        sig: ByteStr,
        pub: ByteStr
    ) =
      for {
        alg    <- algFromCO(digestAlg)
        result <- global.rsaVerify(alg, msg.arr, sig.arr, pub.arr)
      } yield CONST_BOOLEAN(result)

    val rsaVerifyF: BaseFunction[NoContext] = {
      val lim = global.MaxByteStrSizeForVerifyFuncs
      NativeFunction(
        "rsaVerify",
        (if (version < V4) {
           300
         } else {
           1000
         }),
        RSAVERIFY,
        BOOLEAN,
        ("digest", digestAlgorithmType(version)),
        ("message", BYTESTR),
        ("sig", BYTESTR),
        ("pub", BYTESTR)
      ) {
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil
            if version < V4 && msg.size > lim =>
          Left(s"Invalid message size = ${msg.size} bytes, must be not greater than ${lim / 1024} KB")
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
          rsaVerify(digestAlg, msg, sig, pub)
        case xs => notImplemented[Id, EVALUATED](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    def rsaVerifyL(version: StdLibVersion): Array[BaseFunction[NoContext]] = lgen(
      Array(16, 32, 64, 128),
      (n => (s"rsaVerify_${n._1}Kb", (RSAVERIFY_LIM + n._2).toShort)),
      ({
        case 16  => 500
        case 32  => 550
        case 64  => 625
        case 128 => 750
      }),
      (n => {
        case _ :: CONST_BYTESTR(msg: ByteStr) :: _ =>
          Either.cond(msg.size <= n * 1024, (), s"Invalid message size = ${msg.size} bytes, must be not greater than $n KB")
        case xs =>
          notImplemented[Id, Unit](s"rsaVerify_${n}Kb(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }),
      BOOLEAN,
      ("digest", digestAlgorithmType(version)),
      ("message", BYTESTR),
      ("sig", BYTESTR),
      ("pub", BYTESTR)
    ) {
      case (digestAlg: CaseObj) :: CONST_BYTESTR(msg: ByteStr) :: CONST_BYTESTR(sig: ByteStr) :: CONST_BYTESTR(pub: ByteStr) :: Nil =>
        rsaVerify(digestAlg, msg, sig, pub)
      case xs => notImplemented[Id, EVALUATED](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
    }

    def toBase58StringF: BaseFunction[NoContext] =
      NativeFunction(
        "toBase58String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 3L),
        TOBASE58,
        STRING,
        ("bytes", BYTESTR)
      ) {
        case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base58Encode(bytes.arr).flatMap(CONST_STRING(_, reduceLimit = version >= V4))
        case xs                                   => notImplemented[Id, EVALUATED]("toBase58String(bytes: ByteVector)", xs)
      }

    def fromBase58StringF: BaseFunction[NoContext] =
      NativeFunction(
        "fromBase58String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L),
        FROMBASE58,
        BYTESTR,
        ("str", STRING)
      ) {
        case CONST_STRING(str: String) :: Nil => global.base58Decode(str, global.MaxBase58String).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id, EVALUATED]("fromBase58String(str: String)", xs)
      }

    def toBase64StringF: BaseFunction[NoContext] =
      NativeFunction(
        "toBase64String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 35L),
        TOBASE64,
        STRING,
        ("bytes", BYTESTR)
      ) {
        case CONST_BYTESTR(bytes: ByteStr) :: Nil => global.base64Encode(bytes.arr).flatMap(CONST_STRING(_, reduceLimit = version >= V4))
        case xs                                   => notImplemented[Id, EVALUATED]("toBase64String(bytes: ByteVector)", xs)
      }

    def fromBase64StringF: BaseFunction[NoContext] =
      NativeFunction(
        "fromBase64String",
        Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 40L),
        FROMBASE64,
        BYTESTR,
        ("str", STRING)
      ) {
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
          Right(CONST_BOOLEAN(global.merkleVerify(root.arr, proof.arr, value.arr)))
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
        case xs @ (ARR(proof) :: CONST_BYTESTR(value) :: CONST_LONG(index) :: Nil) =>
          if (value.size == 32 && proof.length <= 16 && proof.forall({
                case CONST_BYTESTR(v) => v.size == 32
                case _                => false
              })) {
            CONST_BYTESTR(ByteStr(createRoot(value.arr, Math.toIntExact(index), proof.reverse.map({
              case CONST_BYTESTR(v) => v.arr
              case _                => throw (new Exception("Expect ByteStr"))
            }))))
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

    val bls12Groth16VerifyL: Array[BaseFunction[NoContext]] = lgen(
      (1 to 15).toArray,
      (n => (s"groth16Verify_${n._1}inputs", (BLS12_GROTH16_VERIFY_LIM + n._2).toShort)),
      ({ n =>
        Array(1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600)(n - 1)
      }),
      (n => {
        case _ :: _ :: CONST_BYTESTR(inputs: ByteStr) :: _ =>
          Either.cond(inputs.size <= n * 32, (), s"Invalid inputs size ${inputs.size} bytes, must be not greater than ${n * 32} bytes")
        case xs => notImplemented[Id, Unit](s"groth16Verify_${n}inputs(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      }),
      BOOLEAN,
      ("verifying key", BYTESTR),
      ("proof", BYTESTR),
      ("inputs", BYTESTR)
    ) {
      case CONST_BYTESTR(vk: ByteStr) :: CONST_BYTESTR(proof: ByteStr) :: CONST_BYTESTR(inputs: ByteStr) :: Nil =>
        if (inputs.size > 512)
          Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than 512 bytes")
        else if (inputs.size % 32 != 0)
          Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
        else if (proof.size != 192)
          Left(s"Invalid proof size ${proof.size} bytes, must be equal to 192 bytes")
        else if (vk.size != 48 * (8 + inputs.size / 32))
          Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${(8 + inputs.size / 32) * 48} bytes for ${inputs.size / 32} inputs")
        else
          Right(CONST_BOOLEAN(global.groth16Verify(vk.arr, proof.arr, inputs.arr)))
      case xs => notImplemented[Id, EVALUATED]("groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
    }

    val bn256Groth16VerifyComplexities =
      Array(800, 850, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1550, 1600)

    val bn256Groth16VerifyL: Array[BaseFunction[NoContext]] = lgen(
      (1 to 15).toArray,
      n => (s"bn256Groth16Verify_${n._1}inputs", (BN256_GROTH16_VERIFY_LIM + n._2).toShort),
      n => bn256Groth16VerifyComplexities(n - 1),
      n => {
        case _ :: _ :: CONST_BYTESTR(inputs: ByteStr) :: _ =>
          Either.cond(inputs.size <= n * 32, (), s"Invalid inputs size ${inputs.size} bytes, must be not greater than ${n * 32} bytes")
        case xs => notImplemented[Id, Unit](s"bn256Groth16Verify_${n}inputs(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      },
      BOOLEAN,
      ("verifying key", BYTESTR),
      ("proof", BYTESTR),
      ("inputs", BYTESTR)
    ) {
      case CONST_BYTESTR(vk: ByteStr) :: CONST_BYTESTR(proof: ByteStr) :: CONST_BYTESTR(inputs: ByteStr) :: Nil =>
        if (inputs.size > 512)
          Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than 512 bytes")
        else if (inputs.size % 32 != 0)
          Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
        else if (proof.size != 128)
          Left(s"Invalid proof size ${proof.size} bytes, must be equal to 128 bytes")
        else if (vk.size != inputs.size + 256)
          Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${inputs.size + 256} bytes for ${inputs.size / 32} inputs")
        else
          Right(CONST_BOOLEAN(global.bn256Groth16Verify(vk.arr, proof.arr, inputs.arr)))
      case xs => notImplemented[Id, EVALUATED]("bn256Groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
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
        case CONST_BYTESTR(vk: ByteStr) :: CONST_BYTESTR(proof: ByteStr) :: CONST_BYTESTR(inputs: ByteStr) :: Nil =>
          if (inputs.size > 512)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than 512 bytes")
          else if (inputs.size % 32 != 0)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
          else if (proof.size != 192)
            Left(s"Invalid proof size ${proof.size} bytes, must be equal to 192 bytes")
          else if (vk.size != 48 * (8 + inputs.size / 32))
            Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${(8 + inputs.size / 32) * 48} bytes for ${inputs.size / 32} inputs")
          else
            Right(CONST_BOOLEAN(global.groth16Verify(vk.arr, proof.arr, inputs.arr)))
        case xs => notImplemented[Id, EVALUATED]("groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      }

    val bn256Groth16VerifyF: BaseFunction[NoContext] =
      NativeFunction(
        "bn256Groth16Verify",
        1650,
        BN256_GROTH16_VERIFY,
        BOOLEAN,
        ("verifying key", BYTESTR),
        ("proof", BYTESTR),
        ("inputs", BYTESTR)
      ) {
        case CONST_BYTESTR(vk: ByteStr) :: CONST_BYTESTR(proof: ByteStr) :: CONST_BYTESTR(inputs: ByteStr) :: Nil =>
          if (inputs.size > 512)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than 512 bytes")
          else if (inputs.size % 32 != 0)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
          else if (proof.size != 128)
            Left(s"Invalid proof size ${proof.size} bytes, must be equal to 128 bytes")
          else if (vk.size != inputs.size + 256)
            Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${inputs.size + 256} bytes for ${inputs.size / 32} inputs")
          else
            Right(CONST_BOOLEAN(global.bn256Groth16Verify(vk.arr, proof.arr, inputs.arr)))
        case xs => notImplemented[Id, EVALUATED]("bn256Groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      }

    val ecrecover: BaseFunction[NoContext] =
      NativeFunction(
        "ecrecover",
        70,
        ECRECOVER,
        BYTESTR,
        ("message hash", BYTESTR),
        ("signature", BYTESTR)
      ) {
        case CONST_BYTESTR(messageHash: ByteStr) :: CONST_BYTESTR(signature: ByteStr) :: Nil =>
          if (messageHash.size != 32)
            Left(s"Invalid message hash size ${messageHash.size} bytes, must be equal to 32 bytes")
          else if (signature.size != 65)
            Left(s"Invalid signature size ${signature.size} bytes, must be equal to 65 bytes")
          else
            CONST_BYTESTR(ByteStr(global.ecrecover(messageHash.arr, signature.arr)))
        case xs => notImplemented[Id, EVALUATED]("ecrecover(messageHash:ByteVector, signature:ByteVector)", xs)

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

    val rsaVarNames = List("NOALG", "MD5", "SHA1", "SHA224", "SHA256", "SHA384", "SHA512", "SHA3224", "SHA3256", "SHA3384", "SHA3512")

    val v4RsaDig = rsaHashAlgs(V4)
    val v4Types  = v4RsaDig :+ digestAlgorithmType(V4)
    val v6Types  = v4RsaDig :+ digestAlgorithmType(V6)

    val v4Vars: Map[String, (FINAL, ContextfulVal[NoContext])] =
      rsaVarNames.zip(v4RsaDig.map(t => (t, digestAlgValue(t)))).toMap

    val v3RsaDig = rsaHashAlgs(V3)
    val v3Types  = v3RsaDig :+ digestAlgorithmType(V3)

    val v3Vars: Map[String, (FINAL, ContextfulVal[NoContext])] =
      rsaVarNames.zip(v3RsaDig.map(t => (t, digestAlgValue(t)))).toMap

    val v3Functions =
      Array(
        rsaVerifyF,
        checkMerkleProofF,
        toBase16StringF(checkLength = false),
        fromBase16StringF(checkLength = false)
      )

    def fromV4Functions(version: StdLibVersion) =
      Array(
        bls12Groth16VerifyF,
        bn256Groth16VerifyF,
        createMerkleRootF,
        ecrecover, // new in V4
        rsaVerifyF,
        toBase16StringF(checkLength = true),
        fromBase16StringF(checkLength = true) // from V3
      ) ++ sigVerifyL ++ rsaVerifyL(version) ++ keccak256F_lim ++ blake2b256F_lim ++ sha256F_lim ++ bls12Groth16VerifyL ++ bn256Groth16VerifyL

    val fromV1Ctx = CTX[NoContext](Seq(), Map(), v1Functions)
    val fromV3Ctx = fromV1Ctx |+| CTX[NoContext](v3Types, v3Vars, v3Functions)
    val fromV4Ctx = fromV1Ctx |+| CTX[NoContext](v4Types, v4Vars, fromV4Functions(V4))
    val fromV6Ctx = fromV1Ctx |+| CTX[NoContext](v6Types, v4Vars, fromV4Functions(V6))

    version match {
      case V1 | V2  => fromV1Ctx
      case V3       => fromV3Ctx
      case V4 | V5  => fromV4Ctx
      case _        => fromV6Ctx
    }
  }

  def evalContext[F[_]: Monad](global: BaseGlobal, version: StdLibVersion): EvaluationContext[NoContext, F] =
    build(global, version).evaluationContext[F]

  def compilerContext(global: BaseGlobal, version: StdLibVersion): CompilerContext =
    build(global, version).compilerContext
}
