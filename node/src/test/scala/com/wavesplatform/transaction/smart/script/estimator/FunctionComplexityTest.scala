package com.wavesplatform.transaction.smart.script.estimator

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, _}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.state.diffs.smart.predef.{chainId, scriptWithAllV1Functions}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, Proofs, TxPositiveAmount}
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval

class FunctionComplexityTest(estimator: ScriptEstimator) extends PropSpec {
  private val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, null, DirectiveSet.contractDirectiveSet, ByteStr.empty)

  private def estimate(
      expr: Terms.EXPR,
      ctx: CTX[Environment],
      funcCosts: Map[FunctionHeader, Coeval[Long]]
  ): Either[String, Long] =
    estimator(ctx.evaluationContext(environment).letDefs.keySet, funcCosts, expr)

  private def ctx(version: StdLibVersion): CTX[Environment] = {
    utils.functionCosts(version)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(Global, version).withEnvironment[Environment],
          WavesContext.build(
            Global,
            DirectiveSet(version, Account, Expression).explicitGet(),
            fixBigScriptField = true
          )
        )
      )
  }

  private def getAllFuncExpression(version: StdLibVersion): EXPR = {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr.decodeBase64("YWxpY2U=").get)
    val entry4 = StringDataEntry("str", "test")

    val dtx = DataTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        List(entry1, entry2, entry3, entry4),
        100000,
        1526911531530L,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .explicitGet()

    val recipient = Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet()
    val ttx = TransferTransaction(
      2.toByte,
      PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      recipient,
      Waves,
      TxPositiveAmount.unsafeFrom(100000000),
      Waves,
      TxPositiveAmount.unsafeFrom(100000000),
      ByteStr.decodeBase58("4t2Xazb2SX").get,
      1526641218066L,
      Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get)),
      recipient.chainId
    )

    val script = scriptWithAllV1Functions(dtx, ttx)
    val adaptedScript =
      if (version == V3) script.replace("transactionById", "transferTransactionById")
      else script

    Parser.parseExpr(adaptedScript).get.value
  }

  property("function complexities are correctly defined ") {
    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        ctx(version).functions
          .foreach { function =>
            noException should be thrownBy
              DirectiveDictionary[StdLibVersion].all
                .filter(_ >= version)
                .map(function.costByLibVersion)
          }
      }
  }

  property("estimate script with all functions") {
    def check(version: StdLibVersion, expectedCost: Int) = {
      val expr = ExpressionCompiler(ctx(version).compilerContext, getAllFuncExpression(version)).explicitGet()._1
      estimate(expr, ctx(version), utils.functionCosts(version)) shouldBe Right(expectedCost)
    }

    check(V1, 2317)
    check(V2, 2317)
    check(V3, 1882)
  }
}
