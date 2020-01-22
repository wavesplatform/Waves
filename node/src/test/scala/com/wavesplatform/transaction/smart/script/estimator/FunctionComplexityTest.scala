package com.wavesplatform.transaction.smart.script.estimator

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, _}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.state.diffs.smart.predef.{chainId, scriptWithAllV1Functions}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.encode.Base64

class FunctionComplexityTest(estimator: ScriptEstimator) extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {
  private val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, Coeval(null))

  private def estimate(
    expr: Terms.EXPR,
    ctx: CTX[Environment],
    funcCosts: Map[FunctionHeader, Coeval[Long]]
  ): Either[String, Long] =
    estimator(ctx.evaluationContext(environment).letDefs.keySet, funcCosts, expr)

  private val ctxV1 = {
    utils.functionCosts(V1)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(Global, V1).withEnvironment[Environment],
          CryptoContext.build(Global, V1).withEnvironment[Environment],
          WavesContext.build(
            DirectiveSet(V1, Account, Expression).explicitGet()
          )
        ))
  }

  private val ctxV2 = {
    utils.functionCosts(V2)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(Global, V2).withEnvironment[Environment],
          CryptoContext.build(Global, V2).withEnvironment[Environment],
          WavesContext.build(
            DirectiveSet(V2, Account, Expression).explicitGet()
          )
        ))
  }

  private val ctxV3 = {
    utils.functionCosts(V3)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(Global, V3).withEnvironment[Environment],
          CryptoContext.build(Global, V3).withEnvironment[Environment],
          WavesContext.build(
            DirectiveSet(V3, Account, Expression).explicitGet()
          )
        ))
  }

  private def getAllFuncExpression(version: StdLibVersion): EXPR = {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    val dtx = DataTransaction
      .create(
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        List(entry1, entry2, entry3, entry4),
        100000,
        1526911531530L,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val ttx = TransferTransactionV2
      .create(
        Waves,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").right.get,
        100000000,
        1526641218066L,
        Waves,
        100000000,
        Base58.tryDecodeWithLimit("4t2Xazb2SX").get,
        Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get))
      )
      .right
      .get

    val script = scriptWithAllV1Functions(dtx, ttx)
    val adaptedScript =
      if (version == V3) script.replace("transactionById", "transferTransactionById")
      else script

    Parser.parseExpr(adaptedScript).get.value
  }

  property("func complexity map size is equal stdLib SupportedVersions count") {
    val supportedVersionCount = DirectiveDictionary[StdLibVersion].all.size

    ctxV1.functions.foreach { func =>
      func.costByLibVersion.size shouldBe supportedVersionCount
    }

    ctxV2.functions.foreach { func =>
      func.costByLibVersion.size shouldBe supportedVersionCount
    }

    ctxV3.functions.foreach { func =>
      func.costByLibVersion.size shouldBe supportedVersionCount
    }
  }

  property("estimate script with all functions") {
    val exprV1 = ExpressionCompiler(ctxV1.compilerContext, getAllFuncExpression(V1)).explicitGet()._1
    estimate(exprV1, ctxV1, utils.functionCosts(V1)) shouldBe Right(2317)

    val exprV2 = ExpressionCompiler(ctxV2.compilerContext, getAllFuncExpression(V2)).explicitGet()._1
    estimate(exprV2, ctxV2, utils.functionCosts(V2)) shouldBe Right(2317)

    val exprV3 = ExpressionCompiler(ctxV3.compilerContext, getAllFuncExpression(V3)).explicitGet()._1
    estimate(exprV3, ctxV3, utils.functionCosts(V3)) shouldBe Right(1882)
  }
}
