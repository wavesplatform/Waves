package com.wavesplatform.state.diffs.smart.scenarios

import cats.Id
import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto._
import com.wavesplatform.database
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V4}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ParseFunctionsTest extends PropSpec with PropertyChecks with Matchers {

  val blockheaderGen: Gen[(BlockHeader, ByteStr, Int)] = {
    for {
      timestamp        <- Gen.posNum[Long]
      version          <- Gen.posNum[Byte]
      reference        <- Gen.containerOfN[Array, Byte](SignatureLength, Arbitrary.arbByte.arbitrary)
      generator        <- Gen.containerOfN[Array, Byte](KeyLength, Arbitrary.arbByte.arbitrary)
      signature        <- Gen.containerOfN[Array, Byte](SignatureLength, Arbitrary.arbByte.arbitrary)
      baseTarget       <- Gen.posNum[Long]
      genSignature     <- Gen.containerOfN[Array, Byte](Block.GeneratorSignatureLength, Arbitrary.arbByte.arbitrary)
      transactionCount <- Gen.posNum[Int]
      featureVotes     <- Gen.listOf(Gen.posNum[Short])
      reward           <- Gen.posNum[Long]
    } yield (
        new BlockHeader(
          version,
          timestamp,
          reference,
          baseTarget,
          genSignature,
          PublicKey(ByteStr(generator)),
          featureVotes.toSet,
          reward
        ),
        signature,
        transactionCount
      )
  }

  def scriptSrc(header: BlockHeader, signature: ByteStr, transactionCount: Int): String = {
    val expectedReference    = Base64.encode(header.reference)
    val expectedGenerator    = Base64.encode(header.generator.bytes)
    val expectedGeneratorPK  = Base64.encode(header.generator.toAddress.bytes)
    val expectedSignature    = Base64.encode(signature)
    val expectedGenSignature = Base64.encode(header.generationSignature)

    s"""
      |{-# STDLIB_VERSION  4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |let bytes = base64'${Base64.encode(database.writeBlockHeaderAndSize((header, 1024, transactionCount, signature)))}'
      |
      |match parseBlockHeader(bytes) {
      |  case header: BlockHeader =>
      |    header.timestamp == ${header.timestamp} &&
      |      header.version == ${header.version} &&
      |      header.reference == base64'$expectedReference' &&
      |      header.generator == base64'$expectedGenerator' &&
      |      header.generatorPublicKey == base64'$expectedGeneratorPK' &&
      |      header.signature == base64'$expectedSignature' &&
      |      header.baseTarget == ${header.baseTarget} &&
      |      header.generationSignature == base64'$expectedGenSignature' &&
      |      header.transactionCount == $transactionCount
      |  case _ => throw("Can't parse header")
      |}
      |
    """.stripMargin
  }

  property("should parse blockheader bytes") {
    forAll(blockheaderGen) { data =>
      eval((scriptSrc _).tupled(data)) shouldBe Right(Terms.TRUE)
    }
  }

  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val ds = DirectiveSet(V4, Account, Expression).explicitGet()

    val env = new WavesEnvironment(
      'T',
      Coeval(???),
      Coeval(???),
      EmptyBlockchain,
      Coeval(???),
      ds
    )

    val untyped  = Parser.parseExpr(code).get.value
    val ctx: CTX[Environment] =
      PureContext.build(Global, V4).withEnvironment[Environment]    |+|
      CryptoContext.build(Global, V4) .withEnvironment[Environment] |+|
      WavesContext.build(ds)

    val typed    = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => new EvaluatorV1[Id, Environment].apply(ctx.evaluationContext(env), v._1))
  }

}
