package com.wavesplatform.transaction.smart.script

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, _}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.lang.v1.{CTX, FunctionHeader, ScriptEstimator}
import com.wavesplatform.lang.{ContentType, Global, ScriptType, StdLibVersion}
import com.wavesplatform.state.diffs.smart.predef.scriptWithAllFunctions
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import com.wavesplatform.utils
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.encode.Base64

class FunctionComplexityTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {

  private def estimate(expr: Terms.EXPR, ctx: CTX, funcCosts: Map[FunctionHeader, Coeval[Long]]): Either[String, Long] = {
    ScriptEstimator(ctx.evaluationContext.letDefs.keySet, funcCosts, expr)
  }

  private val ctxV1 = {
    utils.functionCosts(StdLibVersion.V1)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V1),
          CryptoContext.build(Global),
          WavesContext.build(
            DirectiveSet(StdLibVersion.V1, ScriptType.Account, ContentType.Expression).explicitGet(),
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???)),
          )
        ))
  }

  private val ctxV2 = {
    utils.functionCosts(StdLibVersion.V2)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V2),
          CryptoContext.build(Global),
          WavesContext.build(
            DirectiveSet(V2, ScriptType.Account, ContentType.Expression).explicitGet(),
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???))
          )
        ))
  }

  private val ctxV3 = {
    utils.functionCosts(StdLibVersion.V3)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V3),
          CryptoContext.build(Global),
          WavesContext.build(
            DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Expression).explicitGet(),
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???))
          )
        ))
  }

  private def getAllFuncExpression(): EXPR = {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val entry4 = StringDataEntry("str", "test")

    val dtx = DataTransaction
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
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
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
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

    Parser.parseExpr(scriptWithAllFunctions(dtx, ttx)).get.value
  }

  property("func complexity map size is equal stdLib SupportedVersions count") {
    val supportedVersionCount = StdLibVersion.SupportedVersions.size

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

  property("estimate script with with all functions") {
    val exprV1 = ExpressionCompiler(ctxV1.compilerContext, getAllFuncExpression()).explicitGet()._1
    estimate(exprV1, ctxV1, utils.functionCosts(StdLibVersion.V1)) shouldBe Right(2317)

    val exprV2 = ExpressionCompiler(ctxV2.compilerContext, getAllFuncExpression()).explicitGet()._1
    estimate(exprV2, ctxV2, utils.functionCosts(StdLibVersion.V2)) shouldBe Right(2317)

    val exprV3 = ExpressionCompiler(ctxV3.compilerContext, getAllFuncExpression()).explicitGet()._1
    estimate(exprV3, ctxV3, utils.functionCosts(StdLibVersion.V3)) shouldBe Right(2282)
  }
}
