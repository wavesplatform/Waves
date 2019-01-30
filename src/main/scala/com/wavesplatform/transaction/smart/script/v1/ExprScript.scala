package com.wavesplatform.transaction.smart.script.v1

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.Version._
import com.wavesplatform.lang.contract.{Contract, ContractSerDe}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptEstimator, Serde}
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ExprScript.checksumLength
import com.wavesplatform.utils.{functionCosts, varNames}
import monix.eval.Coeval

import scala.annotation.tailrec
import scala.collection.mutable._

object ExprScript {
  val checksumLength         = 4
  private val maxComplexity  = 20 * functionCosts(ExprV1)(FunctionHeader.Native(SIGVERIFY))()
  private val maxSizeInBytes = 8 * 1024

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= maxSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $maxSizeInBytes bytes")

  def apply(x: EXPR): Either[String, Script] = apply(ExprV1, x)

  def apply(version: Version, x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(varNames(version), functionCosts(version), x)
      _                <- Either.cond(scriptComplexity <= maxComplexity, (), s"Script is too complex: $scriptComplexity > $maxComplexity")
      s = new ExprScriprImpl(version, x, scriptComplexity)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  case class ExprScriprImpl(version: Version, expr: EXPR, complexity: Long) extends Script {
    override type Expr = EXPR
    override val text: String = expr.toString
    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(version.toByte) ++ Serde.serialize(expr)
        ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
      }
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(isExprContainsBlockV2(expr))
  }

  def isExprContainsBlockV2(e: EXPR): Boolean = {
    @tailrec
    def horTraversal(queue: MutableList[EXPR]): Boolean = {
      queue.headOption match {
        case Some(expr) =>
          expr match {
            case BLOCK(_, _)                => true
            case GETTER(expr1, _)           => horTraversal(queue.tail += expr1)
            case LET_BLOCK(let, body)       => horTraversal(queue.tail ++ MutableList(let.value, body))
            case IF(expr1, expr2, expr3)    => horTraversal(queue.tail ++ MutableList(expr1, expr2, expr3))
            case FUNCTION_CALL(_, exprList) => horTraversal(queue.tail ++ exprList)
            case _                          => false
          }
        case None => false
      }
    }
    horTraversal(Queue(e))
  }
}

case class ContractScript(version: Version, expr: Contract) extends Script {
  override val complexity: Long = -1
  override type Expr = Contract
  override val text: String = expr.toString
  override val bytes: Coeval[ByteStr] =
    Coeval.evalOnce {
      val s = Array(version.toByte) ++ ContractSerDe.serialize(expr)
      ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
    }
  override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(true)
}
