package com.wavesplatform.lang.evaluator

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, V4}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment.{InputEntity, Tthis}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import shapeless.Coproduct

class EvaluatorV2TestBase extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {
  private val version = V4
  private val ctx =
    PureContext.build(version).withEnvironment[Environment] |+|
      WavesContext.build(DirectiveSet(version, Account, DApp).explicitGet())

  private val environment = new Environment[Id] {
    override def chainId: Byte                                                                                       = ???
    override def inputEntity: InputEntity                                                                            = ???
    override def height: Long                                                                                        = 1
    override def transactionById(id: Array[Byte]): Id[Option[Tx]]                                                    = ???
    override def transferTransactionById(id: Array[Byte]): Id[Option[Tx.Transfer]]                                   = ???
    override def transactionHeightById(id: Array[Byte]): Id[Option[Long]]                                            = ???
    override def assetInfoById(id: Array[Byte]): Id[Option[ScriptAssetInfo]]                                         = ???
    override def lastBlockOpt(): Id[Option[BlockInfo]]                                                               = ???
    override def blockInfoByHeight(height: Int): Id[Option[BlockInfo]]                                               = ???
    override def resolveAlias(name: String): Id[Either[String, Recipient.Address]]                                   = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Id[Either[String, Long]] = ???
    override def accountWavesBalanceOf(addressOrAlias: Recipient): Id[Either[String, Environment.BalanceDetails]]    = ???
    override def multiPaymentAllowed: Boolean                                                                        = ???
    override def txId: ByteStr                                                                                       = ???
    override def transferTransactionFromProto(b: Array[Byte]): Id[Option[Tx.Transfer]]                               = ???
    override def addressFromString(address: String): Either[String, Recipient.Address]                               = ???
    override def callScript(a: Recipient.Address, f: String, e: List[EVALUATED], p: Seq[(Option[Array[Byte]], Long)]): Id[Either[ValidationError, EVALUATED]] = ???

    override def tthis: Tthis =
      Coproduct(Recipient.Address(ByteStr.empty))

    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Id[Option[Any]] = {
      if (key == "unexisting")
        None
      else
        Some(dataType match {
          case DataType.Boolean   => true
          case DataType.Long      => 42L
          case DataType.ByteArray => ByteStr.fill(8)(1)
          case DataType.String    => "foo"
        })
    }
  }

  private val evaluator =
    new EvaluatorV2(LoggedEvaluationContext(_ => _ => (), ctx.evaluationContext(environment)), version)

  protected def evalExpr(expr: EXPR, limit: Int): (EXPR, String, Int) = {
    val (result, unusedComplexity) = evaluator(expr, limit)
    (result, Decompiler(result, ctx.decompilerContext), limit - unusedComplexity)
  }

  protected def eval(script: String, limit: Int): (EXPR, String, Int) =
    evalExpr(compile(script), limit)

  protected def compile(script: String): EXPR = {
    val parsed = Parser.parseExpr(script).get.value
    ExpressionCompiler(ctx.compilerContext, parsed).explicitGet()._1
  }
}
