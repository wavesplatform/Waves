package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.syntax.either.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.{Id, Monad}
import com.wavesplatform.common.merkle.Merkle.createRoot
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.FunctionIds.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, NativeFunction}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

import scala.collection.mutable
import scala.util.Try

object CryptoContext {

  val rsaTypeNames: Seq[String] = List("NoAlg", "Md5", "Sha1", "Sha224", "Sha256", "Sha384", "Sha512", "Sha3224", "Sha3256", "Sha3384", "Sha3512")

  private def rsaHashAlgs(v: StdLibVersion) = {
    rsaTypeNames.map(CASETYPEREF(_, List.empty, v > V3))
  }

  private def digestAlgorithmType(v: StdLibVersion) =
    UNION.create(rsaHashAlgs(v), if (v > V3 && v < V6) Some("RsaDigestAlgs") else None)

  private val rsaHashLib = {
    import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.*
    rsaTypeNames.zip(List(NONE, MD5, SHA1, SHA224, SHA256, SHA384, SHA512, SHA3224, SHA3256, SHA3384, SHA3512)).toMap
  }

  private def algFromCO(obj: Terms.CaseObj): Either[ExecutionError, DigestAlgorithm] = {
    rsaHashLib.get(obj.caseType.name).fold(Left("Unknown digest type"): Either[ExecutionError, DigestAlgorithm])(Right(_))
  }

  private def digestAlgValue(tpe: CASETYPEREF): ContextfulVal[NoContext] =
    ContextfulVal.pure(CaseObj(tpe, Map.empty))

  def build(global: BaseGlobal, version: StdLibVersion, fixEcrecover: Boolean): CTX[NoContext] =
    ctxCache.getOrElse(
      (global, version, fixEcrecover),
      ctxCache.synchronized {
        ctxCache.getOrElseUpdate((global, version, fixEcrecover), buildNew(global, version, fixEcrecover))
      }
    )

  private val ctxCache = mutable.HashMap.empty[(BaseGlobal, StdLibVersion, Boolean), CTX[NoContext]]

  private def buildNew(global: BaseGlobal, version: StdLibVersion, fixEcrecover: Boolean): CTX[NoContext] = {
    def functionFamily(
        startId: Short,
        nameByLimit: Int => String,
        costByLimit: List[(Int, Int)],
        returnType: TYPE,
        args: (String, TYPE)*
    )(body: (Int, List[EVALUATED]) => Either[ExecutionError, EVALUATED]): Array[BaseFunction[NoContext]] =
      costByLimit.mapWithIndex { case ((limit, cost), i) =>
        val name = nameByLimit(limit)
        val id   = (startId + i).toShort
        NativeFunction[NoContext](name, cost, id, returnType, args*)(args => body(limit, args))
      }.toArray

    def hashFunction(name: String, internalName: Short, cost: Long)(h: Array[Byte] => Array[Byte]): BaseFunction[NoContext] =
      NativeFunction(name, cost, internalName, BYTESTR, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(m) :: Nil => CONST_BYTESTR(ByteStr(h(m.arr)))
        case xs                      => notImplemented[Id, EVALUATED](s"$name(bytes: ByteVector)", xs)
      }

    val keccak256F: BaseFunction[NoContext] = {
      val complexity =
        if (version < V4) 10
        else if (version < V6) 200
        else 195
      hashFunction("keccak256", KECCAK256, complexity)(global.keccak256)
    }

    val blake2b256F: BaseFunction[NoContext] = {
      val complexity =
        if (version < V4) 10
        else if (version < V6) 200
        else 136
      hashFunction("blake2b256", BLAKE256, complexity)(global.blake2b256)
    }

    val sha256F: BaseFunction[NoContext] = {
      val complexity = {
        if (version < V4) 10
        else if (version < V6) 200
        else if (version < V9) 118
        else 36
      }
      hashFunction("sha256", SHA256, complexity)(global.sha256)
    }

    def hashLimFunction(
        name: String,
        startId: Short,
        costByLimit: List[(Int, Int)]
    )(hash: Array[Byte] => Array[Byte]): Array[BaseFunction[NoContext]] =
      functionFamily(
        startId,
        limit => s"${name}_${limit}Kb",
        costByLimit,
        BYTESTR,
        ("bytes", BYTESTR)
      ) {
        case (limit, CONST_BYTESTR(msg) :: Nil) =>
          if (msg.size <= limit * 1024)
            CONST_BYTESTR(ByteStr(hash(msg.arr)))
          else
            Left(s"Invalid message size = ${msg.size} bytes, must be not greater than $limit KB")
        case (limit, xs) =>
          notImplemented[Id, EVALUATED](s"${name}_${limit}Kb(bytes: ByteVector)", xs)
      }

    def keccak256F_lim: Array[BaseFunction[NoContext]] =
      hashLimFunction(
        "keccak256",
        KECCAK256_LIM,
        if (version >= V6)
          List(
            (16, 20),
            (32, 39),
            (64, 74),
            (128, 147)
          )
        else
          List(
            (16, 10),
            (32, 25),
            (64, 50),
            (128, 100)
          )
      )(global.keccak256)

    val blake2b256F_lim: Array[BaseFunction[NoContext]] =
      hashLimFunction(
        "blake2b256",
        BLAKE256_LIM,
        if (version >= V6)
          List(
            (16, 13),
            (32, 29),
            (64, 58),
            (128, 115)
          )
        else
          List(
            (16, 10),
            (32, 25),
            (64, 50),
            (128, 100)
          )
      )(global.blake2b256)

    val sha256F_lim: Array[BaseFunction[NoContext]] =
      hashLimFunction(
        "sha256",
        SHA256_LIM,
        if (version >= V9)
          List(
            (16, 5),
            (32, 9),
            (64, 17),
            (128, 32)
          )
        else if (version >= V6)
          List(
            (16, 12),
            (32, 23),
            (64, 47),
            (128, 93)
          )
        else
          List(
            (16, 10),
            (32, 25),
            (64, 50),
            (128, 100)
          )
      )(global.sha256)

    val sigVerifyL: Array[BaseFunction[NoContext]] =
      functionFamily(
        SIGVERIFY_LIM,
        limit => s"sigVerify_${limit}Kb",
        if (version >= V6)
          List(
            (8, 43),
            (16, 50),
            (32, 64),
            (64, 93),
            (128, 150)
          )
        else
          List(
            (8, 47),
            (16, 57),
            (32, 70),
            (64, 102),
            (128, 172)
          ),
        BOOLEAN,
        ("message", BYTESTR),
        ("sig", BYTESTR),
        ("pub", BYTESTR)
      ) {
        case (limit, CONST_BYTESTR(msg) :: CONST_BYTESTR(sig) :: CONST_BYTESTR(pub) :: Nil) =>
          Either.cond(
            msg.size <= limit * 1024,
            CONST_BOOLEAN(global.curve25519verify(msg.arr, sig.arr, pub.arr)),
            s"Invalid message size = ${msg.size} bytes, must be not greater than $limit KB"
          )
        case (limit, xs) =>
          notImplemented[Id, EVALUATED](s"sigVerify_${limit}Kb(message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }

    def sigVerifyF(contextVer: StdLibVersion): BaseFunction[NoContext] = {
      val lim = global.MaxByteStrSizeForVerifyFuncs
      val complexity =
        if (version < V4)
          100
        else if (version < V6)
          200
        else
          180
      NativeFunction("sigVerify", complexity, SIGVERIFY, BOOLEAN, ("message", BYTESTR), ("sig", BYTESTR), ("pub", BYTESTR)) {
        case CONST_BYTESTR(msg) :: CONST_BYTESTR(_) :: CONST_BYTESTR(_) :: Nil if contextVer == V3 && msg.size > lim =>
          Left(s"Invalid message size = ${msg.size} bytes, must be not greater than ${lim / 1024} KB")
        case CONST_BYTESTR(msg) :: CONST_BYTESTR(sig) :: CONST_BYTESTR(pub) :: Nil =>
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
        result <- global.rsaVerify(alg, msg.arr, sig.arr, pub.arr).leftMap(CommonError(_))
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
        case (_: CaseObj) :: CONST_BYTESTR(msg) :: CONST_BYTESTR(_) :: CONST_BYTESTR(_) :: Nil if version < V4 && msg.size > lim =>
          Left(s"Invalid message size = ${msg.size} bytes, must be not greater than ${lim / 1024} KB")
        case (digestAlg: CaseObj) :: CONST_BYTESTR(msg) :: CONST_BYTESTR(sig) :: CONST_BYTESTR(pub) :: Nil =>
          rsaVerify(digestAlg, msg, sig, pub)
        case xs => notImplemented[Id, EVALUATED](s"rsaVerify(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)", xs)
      }
    }

    def rsaVerifyL(version: StdLibVersion): Array[BaseFunction[NoContext]] =
      functionFamily(
        RSAVERIFY_LIM,
        limit => s"rsaVerify_${limit}Kb",
        List(
          (16, 500),
          (32, 550),
          (64, 625),
          (128, 750)
        ),
        BOOLEAN,
        ("digest", digestAlgorithmType(version)),
        ("message", BYTESTR),
        ("sig", BYTESTR),
        ("pub", BYTESTR)
      ) {
        case (limit, (digestAlg: CaseObj) :: CONST_BYTESTR(msg) :: CONST_BYTESTR(sig) :: CONST_BYTESTR(pub) :: Nil) =>
          if (msg.size <= limit * 1024)
            rsaVerify(digestAlg, msg, sig, pub)
          else
            Left(s"Invalid message size = ${msg.size} bytes, must be not greater than $limit KB")
        case (limit, xs) =>
          notImplemented[Id, EVALUATED](
            s"rsaVerify_${limit}Kb(digest: DigestAlgorithmType, message: ByteVector, sig: ByteVector, pub: ByteVector)",
            xs
          )
      }

    def toBase58StringF: BaseFunction[NoContext] =
      NativeFunction(
        "toBase58String",
        Map(V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 3L),
        TOBASE58,
        STRING,
        ("bytes", BYTESTR)
      ) {
        case CONST_BYTESTR(bytes) :: Nil =>
          global.base58Encode(bytes.arr).leftMap(CommonError(_)).flatMap(CONST_STRING(_, reduceLimit = version >= V4))
        case xs => notImplemented[Id, EVALUATED]("toBase58String(bytes: ByteVector)", xs)
      }

    def fromBase58StringF: BaseFunction[NoContext] =
      NativeFunction(
        "fromBase58String",
        Map(V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L),
        FROMBASE58,
        BYTESTR,
        ("str", STRING)
      ) {
        case CONST_STRING(str: String) :: Nil =>
          global.base58Decode(str, global.MaxBase58String).leftMap(CommonError(_)).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs => notImplemented[Id, EVALUATED]("fromBase58String(str: String)", xs)
      }

    def toBase64StringF(
        name: String,
        complexities: Map[StdLibVersion, Long],
        id: Short,
        limit: Int = global.MaxBase64Bytes
    ): BaseFunction[NoContext] =
      NativeFunction(name, complexities, id, STRING, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(bytes) :: Nil =>
          global.base64Encode(bytes.arr, limit).leftMap(CommonError(_)).flatMap(CONST_STRING(_, reduceLimit = version >= V4))
        case xs => notImplemented[Id, EVALUATED]("toBase64String(bytes: ByteVector)", xs)
      }

    val toBase64String    = toBase64StringF("toBase64String", Map(V1 -> 10L, V4 -> 35L, V9 -> 3L), TOBASE64)
    val toBase64String_1C = toBase64StringF("toBase64String_1C", Map(V9 -> 1L), TOBASE64_1C, 1024)

    def fromBase64StringF(
        name: String,
        complexities: Map[StdLibVersion, Long],
        functionId: Short,
        inputSizeLimit: Int = global.MaxBase64String,
        resultSizeLimit: Option[Int] = None
    ): BaseFunction[NoContext] =
      NativeFunction(
        name,
        complexities,
        functionId,
        BYTESTR,
        ("str", STRING)
      ) {
        case CONST_STRING(str: String) :: Nil =>
          for {
            bs <- global.base64Decode(str, inputSizeLimit).leftMap(CommonError(_))
            _ <- resultSizeLimit
              .toLeft(())
              .leftFlatMap { limit =>
                Either.raiseWhen(bs.length > limit)(s"byte vector length ${bs.length} exceeds limit $limit")
              }
              .leftMap(CommonError(_))
            cbs <- CONST_BYTESTR(ByteStr(bs))
          } yield cbs
        case xs => notImplemented[Id, EVALUATED](s"$name(str: String)", xs)
      }

    val fromBase64String    = fromBase64StringF("fromBase64String", Map(V1 -> 10L, V4 -> 40L, V9 -> 12L), FROMBASE64)
    val fromBase64String_1C = fromBase64StringF("fromBase64String_1C", Map(V9 -> 1L), FROMBASE64_1C, global.MaxBase64String_1C, Some(1024))

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
        Map(V4 -> 30L, V9 -> 3L),
        CREATE_MERKLE_ROOT,
        BYTESTR,
        ("merkleProof", LIST(BYTESTR)),
        ("valueBytes", BYTESTR),
        ("index", LONG)
      ) {
        case xs @ ARR(proof) :: CONST_BYTESTR(value) :: CONST_LONG(index) :: Nil =>
          val sizeCheckedProofs = proof.collect { case bs @ CONST_BYTESTR(v) if v.size == 32 => bs }
          if (value.size == 32 && proof.length <= 16 && sizeCheckedProofs.size == proof.size) {
            Try(createRoot(value.arr, Math.toIntExact(index), sizeCheckedProofs.reverse.map(_.bs.arr))).toEither
              .leftMap(e => ThrownError(if (e.getMessage != null) e.getMessage else "error"))
              .flatMap(r => CONST_BYTESTR(ByteStr(r)))
          } else {
            notImplemented[Id, EVALUATED](s"createMerkleRoot(merkleProof: ByteVector, valueBytes: ByteVector, index: Int)", xs)
          }
        case xs => notImplemented[Id, EVALUATED](s"createMerkleRoot(merkleProof: ByteVector, valueBytes: ByteVector, index: Int)", xs)
      }

    def toBase16StringF(name: String, complexities: Map[StdLibVersion, Long], id: Short, limit: Option[Int]): BaseFunction[NoContext] =
      NativeFunction(name, complexities, id, STRING, ("bytes", BYTESTR)) {
        case CONST_BYTESTR(bytes) :: Nil => global.base16Encode(bytes.arr, limit).leftMap(CommonError(_)).flatMap(CONST_STRING(_))
        case xs                          => notImplemented[Id, EVALUATED]("toBase16String(bytes: ByteVector)", xs)
      }

    def toBase16String(checkLength: Boolean) =
      toBase16StringF("toBase16String", Map(V3 -> 10L, V9 -> 4L), TOBASE16, if (checkLength) Some(global.MaxBase16Bytes) else None)
    val toBase16String_1C = toBase16StringF("toBase16String_1C", Map(V9 -> 1L), TOBASE16_1C, Some(1024))

    def fromBase16StringF(name: String, complexities: Map[StdLibVersion, Long], id: Short, limit: Option[Int]): BaseFunction[NoContext] =
      NativeFunction(name, complexities, id, BYTESTR, ("str", STRING)) {
        case CONST_STRING(str: String) :: Nil => global.base16Decode(str, limit).leftMap(CommonError(_)).flatMap(x => CONST_BYTESTR(ByteStr(x)))
        case xs                               => notImplemented[Id, EVALUATED]("fromBase16String(str: String)", xs)
      }

    def fromBase16String(checkLength: Boolean) =
      fromBase16StringF("fromBase16String", Map(V3 -> 10L, V9 -> 4L), FROMBASE16, if (checkLength) Some(global.MaxBase16String) else None)
    val fromBase16String_1C = fromBase16StringF("fromBase16String_1C", Map(V9 -> 1L), FROMBASE16_1C, Some(global.MaxBase16String_1C))

    val bls12Groth16VerifyL: Array[BaseFunction[NoContext]] =
      functionFamily(
        BLS12_GROTH16_VERIFY_LIM,
        limit => s"groth16Verify_${limit}inputs",
        (1200 to 2600 by 100).toList.mapWithIndex { case (complexity, i) => (i + 1, complexity) },
        BOOLEAN,
        ("verifying key", BYTESTR),
        ("proof", BYTESTR),
        ("inputs", BYTESTR)
      ) {
        case (limit, CONST_BYTESTR(vk) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(inputs) :: Nil) =>
          if (inputs.size > limit * 32)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than ${limit * 32} bytes")
          else if (inputs.size % 32 != 0)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
          else if (proof.size != 192)
            Left(s"Invalid proof size ${proof.size} bytes, must be equal to 192 bytes")
          else if (vk.size != 48 * (8 + inputs.size / 32))
            Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${(8 + inputs.size / 32) * 48} bytes for ${inputs.size / 32} inputs")
          else
            Right(CONST_BOOLEAN(global.groth16Verify(vk.arr, proof.arr, inputs.arr)))
        case (limit, xs) =>
          notImplemented[Id, EVALUATED](s"groth16Verify_${limit}inputs(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      }

    val bn256Groth16VerifyL: Array[BaseFunction[NoContext]] = {
      val complexities = List(800, 850, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1550, 1600)
      functionFamily(
        BN256_GROTH16_VERIFY_LIM,
        limit => s"bn256Groth16Verify_${limit}inputs",
        complexities.mapWithIndex { case (complexity, i) => (i + 1, complexity) },
        BOOLEAN,
        ("verifying key", BYTESTR),
        ("proof", BYTESTR),
        ("inputs", BYTESTR)
      ) {
        case (limit, CONST_BYTESTR(vk) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(inputs) :: Nil) =>
          if (inputs.size > limit * 32)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be not greater than ${limit * 32} bytes")
          else if (inputs.size % 32 != 0)
            Left(s"Invalid inputs size ${inputs.size} bytes, must be a multiple of 32 bytes")
          else if (proof.size != 128)
            Left(s"Invalid proof size ${proof.size} bytes, must be equal to 128 bytes")
          else if (vk.size != inputs.size + 256)
            Left(s"Invalid vk size ${vk.size} bytes, must be equal to ${inputs.size + 256} bytes for ${inputs.size / 32} inputs")
          else
            Right(CONST_BOOLEAN(global.bn256Groth16Verify(vk.arr, proof.arr, inputs.arr)))
        case (limit, xs) =>
          notImplemented[Id, EVALUATED](s"bn256Groth16Verify_${limit}inputs(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
      }
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
        case CONST_BYTESTR(vk) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(inputs) :: Nil =>
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
        case xs =>
          notImplemented[Id, EVALUATED]("groth16Verify(vk:ByteVector, proof:ByteVector, inputs:ByteVector)", xs)
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
        case CONST_BYTESTR(vk) :: CONST_BYTESTR(proof) :: CONST_BYTESTR(inputs) :: Nil =>
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
        case CONST_BYTESTR(messageHash) :: CONST_BYTESTR(signature) :: Nil =>
          if (messageHash.size != 32)
            Left(s"Invalid message hash size ${messageHash.size} bytes, must be equal to 32 bytes")
          else if (signature.size != 65)
            Left(s"Invalid signature size ${signature.size} bytes, must be equal to 65 bytes")
          else
            CONST_BYTESTR(ByteStr(global.ecrecover(messageHash.arr, signature.arr, fixEcrecover)))
        case xs => notImplemented[Id, EVALUATED]("ecrecover(messageHash:ByteVector, signature:ByteVector)", xs)

      }

    val validateCertificateChain: BaseFunction[NoContext] =
      NativeFunction(
        "validateCertificateChain",
        43,
        VALIDATE_CERT_CHAIN,
        BYTESTR,
        ("certificateChain", LIST(BYTESTR)),
        ("crls", LIST(BYTESTR)),
        ("timestamp", LONG)
      ) {
        case ARR(certChain) :: ARR(crls) :: CONST_LONG(timestamp) :: Nil =>
          (for {
            _ <- Either.raiseWhen(certChain.isEmpty)("Certificate chain must contain at least one certificate")
            _ <- Either.raiseWhen(certChain.length > 5)("Certificate chain can not contain more than 5 certificates")
            _ <- Either.raiseWhen(certChain.size - 1 != crls.size)(
              s"Certificate chain contains ${certChain.size} certificates and requires ${certChain.size - 1} CRLs, ${crls.size} provided"
            )
            certChainBytes <- Either.cond(
              certChain.forall(_.isInstanceOf[CONST_BYTESTR]),
              certChain.map((_.asInstanceOf[CONST_BYTESTR].bs.arr)),
              "Certificates must be byte vectors"
            )
            crlBytes <- Either.cond(
              crls.forall(_.isInstanceOf[CONST_BYTESTR]),
              crls.map(_.asInstanceOf[CONST_BYTESTR].bs.arr),
              "CRLs must be ByteVectors"
            )

            pk <- global.validateTDXCertChain(certChainBytes, crlBytes, timestamp)
          } yield ByteStr(pk)).left.map(s => CommonError(s)).flatMap(bs => CONST_BYTESTR(bs))
        case xs =>
          notImplemented[Id, EVALUATED](
            "validateCertificateChain(certificateChain:List[ByteVector], crls:List[ByteVector], timestamp:Int)",
            xs
          )
      }

    val secP256verify: BaseFunction[NoContext] =
      NativeFunction(
        "p256Verify",
        43,
        SECP256VERIFY,
        BOOLEAN,
        ("message", BYTESTR),
        ("signature", BYTESTR),
        ("publicKey", BYTESTR)
      ) {
        case CONST_BYTESTR(msg) :: CONST_BYTESTR(signature) :: CONST_BYTESTR(pk) :: Nil =>
          (for {
            res <- global.p256verify(msg.arr, signature.arr, pk.arr)
          } yield CONST_BOOLEAN(res)).left.map(s => CommonError(s))
        case xs =>
          notImplemented[Id, EVALUATED](
            "p256Verify(message:ByteVector, signature:ByteVector, publicKey:ByteVector)",
            xs
          )
      }

    val v1Functions =
      Array(
        keccak256F,
        blake2b256F,
        sha256F,
        sigVerifyF(version),
        toBase58StringF,
        fromBase58StringF,
        toBase64String,
        fromBase64String
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
        toBase16String(checkLength = false),
        fromBase16String(checkLength = false)
      )

    def fromV4Functions(version: StdLibVersion) =
      Array(
        bls12Groth16VerifyF,
        bn256Groth16VerifyF,
        createMerkleRootF,
        ecrecover, // new in V4
        rsaVerifyF,
        toBase16String(checkLength = true),
        fromBase16String(checkLength = true) // from V3
      ) ++ sigVerifyL ++ rsaVerifyL(version) ++ keccak256F_lim ++ blake2b256F_lim ++ sha256F_lim ++ bls12Groth16VerifyL ++ bn256Groth16VerifyL

    val fromV9Functions = fromV4Functions(V9) ++ Array(
      fromBase64String_1C,
      fromBase16String_1C,
      toBase64String_1C,
      toBase16String_1C,
      secP256verify,
      validateCertificateChain
    )

    val fromV1Ctx = CTX[NoContext](Seq(), Map(), v1Functions)
    val fromV3Ctx = fromV1Ctx |+| CTX[NoContext](v3Types, v3Vars, v3Functions)
    val fromV4Ctx = fromV1Ctx |+| CTX[NoContext](v4Types, v4Vars, fromV4Functions(V4))
    val fromV6Ctx = fromV1Ctx |+| CTX[NoContext](v6Types, v4Vars, fromV4Functions(V6))
    val fromV9Ctx = fromV1Ctx |+| CTX[NoContext](v6Types, v4Vars, fromV9Functions)

    version match {
      case V1 | V2      => fromV1Ctx
      case V3           => fromV3Ctx
      case V4 | V5      => fromV4Ctx
      case V6 | V7 | V8 => fromV6Ctx
      case V9           => fromV9Ctx
    }
  }

  def evalContext[F[_]: Monad](global: BaseGlobal, version: StdLibVersion, fixEcrecover: Boolean): EvaluationContext[NoContext, F] =
    build(global, version, fixEcrecover).evaluationContext[F]

  def compilerContext(global: BaseGlobal, version: StdLibVersion, fixEcrecover: Boolean): CompilerContext =
    build(global, version, fixEcrecover).compilerContext
}
