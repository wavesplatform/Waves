package com.wavesplatform.lang.v1

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.{AorBorC, NoShrink, addCtx, sampleTypes}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class FoldTest extends PropSpec with PropertyChecks with Matchers with NoShrink {
  private val emptyEnv = new Environment {
    lazy val unavailable = throw new RuntimeException(s"Blockchain state is unavailable")
    override def height: Long                                                                                    = 0
    override def chainId: Byte                                                                                   = 0
    override def inputEntity: InputEntity                                                                        = unavailable
    override def tthis: Recipient.Address                                                                        = unavailable
    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = unavailable
    override def transferTransactionById(id: Array[Byte]): Option[Tx]                                            = unavailable
    override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = unavailable
    override def assetInfoById(d: Array[Byte]): Option[ScriptAssetInfo]                                          = unavailable
    override def lastBlockOpt(): Option[BlockInfo]                                                               = unavailable
    override def blockInfoByHeight(height: Int): Option[BlockInfo]                                               = unavailable
    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = unavailable
    override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = unavailable
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = unavailable
  }

  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val untyped                                                = Parser.parseExpr(code).get.value
    val ctx: CTX =
      Monoid.combineAll(Seq(
        PureContext.build(Global, V3),
        WavesContext.build(DirectiveSet.contractDirectiveSet, emptyEnv),
        CryptoContext.build(Global, V3)
      ))
    val typed = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => EvaluatorV1[T](ctx.evaluationContext, v._1))
  }

  property("sum") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<5>(arr, 9, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_LONG(1 + 2 + 3 + 4 + 5 + 9))
  }

  property("all is odd") {
    val script =
      s"""
         | func checkOdd(acc: Boolean, a: Int) = acc && (a % 2 == 1)
         | let arr = [1, 3, 5, 7]
         | FOLD<5>(arr, true, checkOdd)
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<4>(arr, 9, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Left("List size exceed 4")
  }

  property("FOLD as FOLD param") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<5>(arr, FOLD<5>(arr, 9, sum), sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_LONG(9 + 2 * (1 + 2 + 3 + 4 + 5)))
  }
}
