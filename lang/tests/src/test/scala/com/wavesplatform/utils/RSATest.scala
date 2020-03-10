package com.wavesplatform.utils

import java.security.{KeyPair, KeyPairGenerator, SecureRandom, Signature}

import cats.Id
import cats.syntax.monoid._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA._
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.Common.produce
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
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

  def scriptSrc(alg: DigestAlgorithm, msg: Array[Byte], sig: Array[Byte], pub: Array[Byte]): String = {
    s"""
       |let msg = base64'${Base64.encode(msg)}'
       |let sig = base64'${Base64.encode(sig)}'
       |let pub = base64'${Base64.encode(pub)}'
       |
       |rsaVerify(${algToType(alg)}(), msg, sig, pub) && rsaVerify(${algToType(alg).toUpperCase}, msg, sig, pub)
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

        eval(scriptSrc(alg, message, signature, xpub.getEncoded)) shouldBe Right(CONST_BOOLEAN(true))
      }


    }
  }

  property("false on incorrect signature") {
    forAll(keyPairGenerator, messageGenerator) { (keyPair, message) =>
      val xpub = keyPair.getPublic

      val signature = new Array[Byte](256)
      Random.nextBytes(signature)

      algs foreach { alg =>
        eval(scriptSrc(alg, message, signature, xpub.getEncoded)) shouldBe Right(CONST_BOOLEAN(false))
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

  property("rsaVerify illegal params") {
    eval("rsaVerify(SHA512, base58'2345', base58'2345', base58'2345')") should produce("Illegal input params")
  }

  private val evaluator = new EvaluatorV1[Id, NoContext]()

  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val untyped      = Parser.parseExpr(code).get.value
    val ctx: CTX[NoContext] = PureContext.build(Global, V3) |+| CryptoContext.build(Global, V3)
    val typed    = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => evaluator.apply[T](ctx.evaluationContext, v._1))
  }

}
