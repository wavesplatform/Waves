package com.wavesplatform.utils

import java.security.{KeyPair, KeyPairGenerator, SecureRandom, Signature}

import cats.syntax.monoid._
import com.wavesplatform.common.crypto.RSA
import com.wavesplatform.common.crypto.RSA._
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class RSATest extends PropSpec with PropertyChecks with Matchers with BeforeAndAfterAll {

  lazy val provider = new BouncyCastleProvider

  val keyPairGenerator: Gen[KeyPair] = {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(2048, new SecureRandom)

    Gen.oneOf(Seq(generator.generateKeyPair))
  }

  val messageGenerator: Gen[Array[Byte]] =
    Gen.asciiPrintableStr
      .map(_.getBytes("UTF-8"))

  val algGenerator: Gen[DigestAlgorithm] =
    Gen
      .oneOf(
        NONE,
        MD2,
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

  def scriptSrc(alg: DigestAlgorithm, msg: Array[Byte], sig: Array[Byte], pub: Array[Byte]): String = {
    val algStr = alg match {
      case NONE    => "NOALG"
      case MD2     => "MD2"
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

    s"""
       |
       |let msg = base64'${Base64.encode(msg)}'
       |let sig = base64'${Base64.encode(sig)}'
       |let pub = base64'${Base64.encode(pub)}'
       |
       |rsaVerify($algStr, msg, sig, pub)
       |
        """.stripMargin
  }

  property("true on correct signature") {
    forAll(keyPairGenerator, messageGenerator, algGenerator) { (keyPair, message, alg) =>
      val xpub = keyPair.getPublic
      val xprv = keyPair.getPrivate

      val prefix = RSA.digestAlgorithmPrefix(alg)

      val privateSignature = Signature.getInstance(s"${prefix}withRSA", provider)

      privateSignature.initSign(xprv)
      privateSignature.update(message)

      val signature = privateSignature.sign

      eval(scriptSrc(alg, message, signature, xpub.getEncoded)) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("false on incorrect signature") {
    forAll(keyPairGenerator, messageGenerator, algGenerator) { (keyPair, message, alg) =>
      val xpub = keyPair.getPublic

      val signature = new Array[Byte](256)

      Random.nextBytes(signature)
      eval(scriptSrc(alg, message, signature, xpub.getEncoded)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val untyped  = Parser.parseExpr(code).get.value
    val ctx: CTX = PureContext.build(V3) |+| CryptoContext.build(Global, V3)
    val typed    = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => EvaluatorV1[T](ctx.evaluationContext, v._1))
  }

}
