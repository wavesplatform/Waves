package com.wavesplatform.utils

import java.security.{KeyPair, KeyPairGenerator, SecureRandom, Signature}

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContextfulVal, EvaluatorV1}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.test._
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._

import scala.util.Random

class RSATest extends PropSpec with BeforeAndAfterAll {

  lazy val provider = new BouncyCastleProvider

  val keyPairGenerator: Gen[KeyPair] = {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(2048, new SecureRandom)

    Gen.oneOf(Seq(generator.generateKeyPair))
  }

  val messageGenerator: Gen[Array[Byte]] =
    Gen.asciiPrintableStr
      .map(_.getBytes("UTF-8"))

  def sizedMessageGenerator(len: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)

  val algs = List(
    NONE,
    MD5,
    SHA1,
    SHA224,
    SHA256,
    SHA384,
    SHA512,
    SHA3224,
    SHA3256,
    SHA3384,
    SHA3512
  )

  def algToType(alg: DigestAlgorithm): String = alg match {
    case NONE    => "NoAlg"
    case MD5     => "Md5"
    case SHA1    => "Sha1"
    case SHA224  => "Sha224"
    case SHA256  => "Sha256"
    case SHA384  => "Sha384"
    case SHA512  => "Sha512"
    case SHA3224 => "Sha3224"
    case SHA3256 => "Sha3256"
    case SHA3384 => "Sha3384"
    case SHA3512 => "Sha3512"
  }

  def algToVar(alg: DigestAlgorithm): String = alg match {
    case NONE    => "NOALG"
    case MD5     => "MD5"
    case SHA1    => "SHA1"
    case SHA224  => "SHA224"
    case SHA256  => "SHA256"
    case SHA384  => "SHA384"
    case SHA512  => "SHA512"
    case SHA3224 => "SHA3224"
    case SHA3256 => "SHA3256"
    case SHA3384 => "SHA3384"
    case SHA3512 => "SHA3512"
  }

  def scriptSrc(alg: DigestAlgorithm, msg: Array[Byte], sig: Array[Byte], pub: Array[Byte]): String = {
    s"""
       |let msg = base64'${Base64.encode(msg)}'
       |let sig = base64'${Base64.encode(sig)}'
       |let pub = base64'${Base64.encode(pub)}'
       |
       |rsaVerify(${algToType(alg)}(), msg, sig, pub) && rsaVerify(${algToType(alg).toUpperCase}, msg, sig, pub)
        """.stripMargin
  }

  def limScriptSrc(lim: Int, alg: DigestAlgorithm, sig: Array[Byte], pub: Array[Byte]): String = {
    s"""
       |let sig = base64'${Base64.encode(sig)}'
       |let pub = base64'${Base64.encode(pub)}'
       |
       |rsaVerify_${lim}Kb(${algToVar(alg)}, msg, sig, pub) && rsaVerify_${lim}Kb(${algToType(alg).toUpperCase}, msg, sig, pub)
        """.stripMargin
  }

  def maxScriptSrc(alg: DigestAlgorithm, sig: Array[Byte], pub: Array[Byte]): String = {
    s"""
       |let sig = base64'${Base64.encode(sig)}'
       |let pub = base64'${Base64.encode(pub)}'
       |
       |rsaVerify(${algToType(alg)}(), msg, sig, pub) && rsaVerify(${algToVar(alg).toUpperCase}, msg, sig, pub)
        """.stripMargin
  }

  def maxScriptSrcV4(alg: DigestAlgorithm, sig: Array[Byte], pub: Array[Byte]): String = {
    s"""
       |let sig = base64'${Base64.encode(sig)}'
       |let pub = base64'${Base64.encode(pub)}'
       |
       |rsaVerify(${algToVar(alg).toUpperCase}, msg, sig, pub)
        """.stripMargin
  }

  property("true on correct signature") {
    forAll(keyPairGenerator, messageGenerator) { (keyPair, message) =>
      val xpub = keyPair.getPublic
      val xprv = keyPair.getPrivate

      algs foreach { alg =>
        val prefix = RSA.digestAlgorithmPrefix(alg)

        val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
        privateSignature.initSign(xprv)
        privateSignature.update(message)

        val signature = privateSignature.sign

        eval(
          scriptSrc(alg, message, signature, xpub.getEncoded),
          PureContext.build(V3, useNewPowPrecision = true) |+| CryptoContext.build(Global, V3)
        ) shouldBe Right(CONST_BOOLEAN(true))
      }
    }
  }

  property("RsaDigestAlgs disabled in V4") {
    forAll(keyPairGenerator, messageGenerator) { (keyPair, message) =>
      val xpub = keyPair.getPublic
      val xprv = keyPair.getPrivate

      algs foreach { alg =>
        val prefix = RSA.digestAlgorithmPrefix(alg)

        val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
        privateSignature.initSign(xprv)
        privateSignature.update(message)

        val signature = privateSignature.sign

        eval(
          scriptSrc(alg, message, signature, xpub.getEncoded),
          PureContext.build(V4, useNewPowPrecision = true) |+| CryptoContext.build(Global, V4)
        ) should produce(s"Can't find a function '${algToType(alg)}'()")
      }
    }
  }

  property("rsaVerify_*Kb work with max size") {
    for (lim <- Seq(16, 32, 64, 128)) {
      forAll(keyPairGenerator, sizedMessageGenerator(lim * 1024)) { (keyPair, message) =>
        val xpub = keyPair.getPublic
        val xprv = keyPair.getPrivate

        algs.filter(_ != NONE) foreach { alg =>
          val prefix = RSA.digestAlgorithmPrefix(alg)

          val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
          privateSignature.initSign(xprv)
          privateSignature.update(message)

          val signature = privateSignature.sign

          val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
            ("msg", (BYTESTR, ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(message), limit = CONST_BYTESTR.DataTxSize).explicitGet())))
          )
          val ctx: CTX[NoContext] = PureContext.build(V4, useNewPowPrecision = true) |+| CryptoContext.build(Global, V4) |+| CTX[NoContext](
            Seq(),
            vars,
            Array.empty[BaseFunction[NoContext]]
          )

          eval(limScriptSrc(lim, alg, signature, xpub.getEncoded), ctx) shouldBe Right(CONST_BOOLEAN(true))
        }
      }
    }
  }

  property("rsaVerify_*Kb fail with max+1 size") {
    for (lim <- Seq(16, 32, 64, 128)) {
      forAll(keyPairGenerator, sizedMessageGenerator(lim * 1024 + 1)) { (keyPair, message) =>
        val xpub = keyPair.getPublic
        val xprv = keyPair.getPrivate

        algs.filter(_ != NONE) foreach { alg =>
          val prefix = RSA.digestAlgorithmPrefix(alg)

          val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
          privateSignature.initSign(xprv)
          privateSignature.update(message)

          val signature = privateSignature.sign

          val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
            ("msg", (BYTESTR, ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(message), limit = CONST_BYTESTR.DataTxSize).explicitGet())))
          )
          val ctx: CTX[NoContext] = PureContext.build(V4, useNewPowPrecision = true) |+| CryptoContext.build(Global, V4) |+| CTX[NoContext](
            Seq(),
            vars,
            Array.empty[BaseFunction[NoContext]]
          )

          eval(limScriptSrc(lim, alg, signature, xpub.getEncoded), ctx) shouldBe Left(
            s"Invalid message size = ${lim * 1024 + 1} bytes, must be not greater than ${lim} KB"
          )
        }
      }
    }
  }

  property("rsaVerify works with max size V4") {
    forAll(keyPairGenerator, sizedMessageGenerator(DataTxMaxBytes)) { (keyPair, message) =>
      val xpub = keyPair.getPublic
      val xprv = keyPair.getPrivate

      algs.filter(_ != NONE) foreach { alg =>
        val prefix = RSA.digestAlgorithmPrefix(alg)

        val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
        privateSignature.initSign(xprv)
        privateSignature.update(message)

        val signature = privateSignature.sign

        val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
          ("msg", (BYTESTR, ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(message), limit = CONST_BYTESTR.DataTxSize).explicitGet())))
        )
        val ctx: CTX[NoContext] = PureContext.build(V4, useNewPowPrecision = true) |+| CryptoContext.build(Global, V4) |+| CTX[NoContext](
          Seq(),
          vars,
          Array.empty[BaseFunction[NoContext]]
        )

        eval(maxScriptSrcV4(alg, signature, xpub.getEncoded), ctx) shouldBe Right(CONST_BOOLEAN(true))
      }
    }
  }

  property("rsaVerify works with max size V3") {
    forAll(keyPairGenerator, sizedMessageGenerator(32 * 1024)) { (keyPair, message) =>
      val xpub = keyPair.getPublic
      val xprv = keyPair.getPrivate

      algs.filter(_ != NONE) foreach { alg =>
        val prefix = RSA.digestAlgorithmPrefix(alg)

        val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
        privateSignature.initSign(xprv)
        privateSignature.update(message)

        val signature = privateSignature.sign

        val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
          ("msg", (BYTESTR, ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(message), limit = CONST_BYTESTR.DataTxSize).explicitGet())))
        )
        val ctx: CTX[NoContext] = PureContext.build(V3, useNewPowPrecision = true) |+| CryptoContext.build(Global, V3) |+| CTX[NoContext](
          Seq(),
          vars,
          Array.empty[BaseFunction[NoContext]]
        )

        eval(maxScriptSrc(alg, signature, xpub.getEncoded), ctx) shouldBe Right(CONST_BOOLEAN(true))
      }
    }
  }

  property("rsaVerify fails with max+1 size V3") {
    forAll(keyPairGenerator, sizedMessageGenerator(32 * 1024 + 1)) { (keyPair, message) =>
      val xpub = keyPair.getPublic
      val xprv = keyPair.getPrivate

      algs.filter(_ != NONE) foreach { alg =>
        val prefix = RSA.digestAlgorithmPrefix(alg)

        val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
        privateSignature.initSign(xprv)
        privateSignature.update(message)

        val signature = privateSignature.sign

        val vars: Map[String, (FINAL, ContextfulVal[NoContext])] = Map(
          ("msg", (BYTESTR, ContextfulVal.pure[NoContext](CONST_BYTESTR(ByteStr(message), limit = CONST_BYTESTR.DataTxSize).explicitGet())))
        )
        val ctx: CTX[NoContext] = PureContext.build(V3, useNewPowPrecision = true) |+| CryptoContext.build(Global, V3) |+| CTX[NoContext](
          Seq(),
          vars,
          Array.empty[BaseFunction[NoContext]]
        )

        eval(maxScriptSrc(alg, signature, xpub.getEncoded), ctx) shouldBe Left(
          s"Invalid message size = ${32 * 1024 + 1} bytes, must be not greater than 32 KB"
        )
      }
    }
  }

  property("false on incorrect signature") {
    forAll(keyPairGenerator, messageGenerator) { (keyPair, message) =>
      val xpub = keyPair.getPublic

      val signature = new Array[Byte](256)
      Random.nextBytes(signature)

      algs foreach { alg =>
        eval(
          scriptSrc(alg, message, signature, xpub.getEncoded),
          PureContext.build(V3, useNewPowPrecision = true) |+| CryptoContext.build(Global, V3)
        ) shouldBe Right(CONST_BOOLEAN(false))
      }
    }
  }

  property("can't compile instantiating from const") {
    forAll(keyPairGenerator, messageGenerator) { (keyPair, message) =>
      def wrongScriptSrc(algConst: String): String = {
        s"rsaVerify($algConst(), base64'', base64'', base64'')".stripMargin
      }

      algs foreach { alg =>
        val const = algToType(alg).toUpperCase
        eval(wrongScriptSrc(const)) should produce(s"Can't find a function '$const'() or it is @Callable")
      }
    }
  }

  property("rsaVerify invalid key") {
    forAll(keyPairGenerator, messageGenerator) { (keyPair, message) =>
      val xprv = keyPair.getPrivate

      algs foreach { alg =>
        val prefix = RSA.digestAlgorithmPrefix(alg)

        val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)
        privateSignature.initSign(xprv)
        privateSignature.update(message)

        val signature = privateSignature.sign
        val ctx       = PureContext.build(V3, useNewPowPrecision = true) |+| CryptoContext.build(Global, V3)

        val invalidKey = Array[Byte](1, 2, 3)
        eval(scriptSrc(alg, message, signature, invalidKey), ctx) should produce(s"Invalid key base58'${ByteStr(invalidKey)}'")
      }
    }
  }

  private val evaluator = new EvaluatorV1[Id, NoContext]()

  private def eval[T <: EVALUATED](
      code: String,
      ctx: CTX[NoContext] = PureContext.build(V4, useNewPowPrecision = true) |+| CryptoContext.build(Global, V4)
  ): Either[String, T] = {
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(ctx.compilerContext, V4, untyped)
    typed.flatMap(v => evaluator[T](ctx.evaluationContext, v._1).leftMap(_.message))
  }

}
