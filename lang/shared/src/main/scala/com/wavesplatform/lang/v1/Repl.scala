package com.wavesplatform.lang.v1

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import monix.execution.atomic.{Atomic, AtomicAny}
import cats.implicits._
import com.wavesplatform.lang.directives.DirectiveSet.contractDirectiveSet
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Expressions.{BLOCK, EXPR}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}

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
  }
  private val ctx = CryptoContext.build(Global, ver) |+| PureContext.build(Global, ver) |+| WavesContext.build(contractDirectiveSet, emptyBlockchainEnv)
  private val initialState = (e: EXPR) => e
  private val scriptAcc = Atomic(initialState)

  def execute(expr: String): Either[String, String] =
    transformAndExtract(scriptAcc, (acc: EXPR => EXPR) => {
      evalExpr(expr, acc) match {
        case Left(e)            => (Left(e), acc)
        case Right((r, newAcc)) => (Right(r), newAcc)
      }
    })

  private def transformAndExtract[S <: AnyRef, R](s: AtomicAny[S], f: S => (R, S)): R = {
    val (result, nextState) = f(s.get)
    s.set(nextState)
    result
  }

  private def evalExpr(expr: String, acc: EXPR => EXPR): Either[String, (String, EXPR => EXPR)] =
    for {
      parsed <- Parser.parseExprOrDecl(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )
      exprWithState = acc(parsed)
      (compiled, _) <- tryEi(ExpressionCompiler(ctx.compilerContext, exprWithState))
      eval          <- tryEi(EvaluatorV1[EVALUATED](ctx.evaluationContext, compiled))
    } yield (eval.prettyString(0), extractDecl(exprWithState, identity))

  private def extractDecl(expr: EXPR, decl: EXPR => EXPR): EXPR => EXPR =
    expr match {
      case BLOCK(p, nextDecl, root) => extractDecl(root, decl compose (BLOCK(p, nextDecl, _)))
      case _                        => decl
    }

  private def tryEi[R](r: => Either[String, R]): Either[String, R] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatten

  def clear(): Unit = scriptAcc.set(initialState)
}
