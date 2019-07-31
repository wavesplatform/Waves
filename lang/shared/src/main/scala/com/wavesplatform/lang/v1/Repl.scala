package com.wavesplatform.lang.v1

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import monix.execution.atomic.Atomic
import cats.implicits._
import com.wavesplatform.lang.directives.DirectiveSet.contractDirectiveSet
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.domain.{BlockHeader, BlockInfo, Recipient, ScriptAssetInfo, Tx}

import scala.util.Try

case class Repl(ver: StdLibVersion = V3) {
  private val Global: BaseGlobal = com.wavesplatform.lang.Global
  private val emptyBlockchainEnv = new Environment {
    lazy val unavailable = throw new RuntimeException(s"Blockchain state is unavailable from REPL")
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
    override def blockHeaderParser(bytes: Array[Byte]): Option[BlockHeader]                                      = unavailable
  }
  private val ctx = CryptoContext.build(Global, ver) |+| PureContext.build(Global, ver) |+| WavesContext.build(contractDirectiveSet, emptyBlockchainEnv)
  private val scriptAcc = Atomic("")

  def execute(expr: String): Either[String, String] =
    scriptAcc.transformAndExtract(acc => {
      val newScript = acc + "\n" + expr
      evalExpr(newScript) match {
        case Left(e)          => (Left(e), acc)
        case Right((r, UNIT)) => (Right(r), newScript)
        case Right((r, _))    => (Right(r), acc)
      }
    })

  private def evalExpr(expr: String): Either[String, (String, FINAL)] =
    for {
      parsed <- Parser.parseExprOrDecl(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )
      (compiled, cType) <- tryEi(ExpressionCompiler(ctx.compilerContext, parsed))
      eval              <- tryEi(EvaluatorV1[EVALUATED](ctx.evaluationContext, compiled))
    } yield (eval.prettyString(0), cType)

  private def tryEi[R](r: => Either[String, R]): Either[String, R] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatten
}
