package com.wavesplatform.state.diffs.smart.scenarios

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.{Block, BlockHeader, SignerData}
import com.wavesplatform.common.utils._
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V4}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import com.wavesplatform.crypto._

class ParseFunctionsTest extends PropSpec with PropertyChecks with Matchers {

  val blockheaderGen: Gen[BlockHeader] = {
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
    } yield {
      new BlockHeader(
        timestamp,
        version,
        reference,
        SignerData(PublicKey(generator), signature),
        NxtLikeConsensusBlockData(baseTarget, genSignature),
        transactionCount,
        featureVotes.toSet
      )
    }
  }

  def scriptSrc(header: BlockHeader): String = {
    val expectedReference    = Base64.encode(header.reference)
    val expectedGenerator    = Base64.encode(header.signerData.generator.bytes)
    val expectedGeneratorPK  = Base64.encode(header.signerData.generator.toAddress.bytes)
    val expectedSignature    = Base64.encode(header.signerData.signature)
    val expectedGenSignature = Base64.encode(header.consensusData.generationSignature)

    s"""
      |{-# STDLIB_VERSION  4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |let bytes = base64'${Base64.encode(BlockHeader.writeHeaderOnly(header))}'
      |
      |match parseBlockHeader(bytes) {
      |  case header: BlockHeader =>
      |    header.timestamp == ${header.timestamp} &&
      |      header.version == ${header.version} &&
      |      header.reference == base64'$expectedReference' &&
      |      header.generator == base64'$expectedGenerator' &&
      |      header.generatorPublicKey == base64'$expectedGeneratorPK' &&
      |      header.signature == base64'$expectedSignature' &&
      |      header.baseTarget == ${header.consensusData.baseTarget} &&
      |      header.generationSignature == base64'$expectedGenSignature' &&
      |      header.transactionCount == ${header.transactionCount}
      |  case _ => throw("Can't parse header")
      |}
      |
    """.stripMargin
  }

  property("should parse blockheader bytes") {
    forAll(blockheaderGen) { header =>
      eval(scriptSrc(header)) shouldBe Right(Terms.TRUE)
    }
  }

  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val ds = DirectiveSet(V4, Account, Expression).explicitGet()

    val env = new WavesEnvironment(
      'T',
      Coeval(???),
      Coeval(???),
      EmptyBlockchain,
      Coeval(???)
    )

    val untyped  = Parser.parseExpr(code).get.value
    val ctx: CTX = PureContext.build(Global, V4) |+| CryptoContext.build(Global, V4) |+| WavesContext.build(ds, env)
    val typed    = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => EvaluatorV1[T](ctx.evaluationContext, v._1))
  }

}
