package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.VerifierFunction
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Bindings, Types}
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports.raiseError
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Tx}
import scodec.bits.ByteVector

import scala.collection.mutable.ListBuffer

object ContractEvaluator {
  case class Invokation(name: String, fc: FUNCTION_CALL, invoker: ByteVector)

  def eval(c: Contract, i: Invokation): EvalM[EVALUATED] = {
    c.cfs.find(_.u.name == i.name) match {
      case None => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"Callable function '${i.name} doesn't exist in the contract")
      case Some(f) =>
        val zeroExpr = BLOCKV2(
          LET(f.c.pubKeyArgName, CONST_BYTEVECTOR(i.invoker)),
          BLOCKV2(f.u, i.fc)
        )
        val expr = c.dec.foldRight(zeroExpr) { (d, e) =>
          BLOCKV2(d, e)
        }
        EvaluatorV1.evalExpr(expr)
    }
  }

  def verify(v: VerifierFunction, tx: Tx): EvalM[EVALUATED] = {
    val t = Bindings.transactionObject(tx, proofsEnabled = true)
    val expr =
      BLOCKV2(LET(v.v.txArgName, t), BLOCKV2(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }

  def apply(ctx: EvaluationContext, c: Contract, i: Invokation): Either[ExecutionError, ContractResult] = {
    val log = ListBuffer[LogItem]()
    val llc = (str: String) => (v: LetExecResult) => log.append((str, v))
    val lec = LoggedEvaluationContext(llc, ctx)
    println(log mkString "\n")
    eval(c, i).run(lec).value._2.flatMap(ContractResult.fromObj)

  }
}

case class ContractResult(ds: List[DataItem[_]], ts: List[(Address, Long)])
object ContractResult {
  def fromObj(e: EVALUATED): Either[ExecutionError, ContractResult] = {
    e match {
      case CaseObj(tpe, fields) if tpe.name == "WriteSet" =>
        val xs: IndexedSeq[EVALUATED] = fields("data").asInstanceOf[ARR].xs
        val r: IndexedSeq[DataItem[_]] = xs.map {
          case CaseObj(tpe, fields) if tpe.name == "DataEntry" =>
            (fields("key"), fields("value")) match {
              case (CONST_STRING(k), CONST_BOOLEAN(b))    => DataItem.Bool(k, b)
              case (CONST_STRING(k), CONST_STRING(b))     => DataItem.Str(k, b)
              case (CONST_STRING(k), CONST_LONG(b))       => DataItem.Lng(k, b)
              case (CONST_STRING(k), CONST_BYTEVECTOR(b)) => DataItem.Bin(k, b)
              case _                                      => ???
            }
          case _ => ???
        }
        Right(ContractResult(r.toList, List.empty))
      case CaseObj(tpe, fields) if tpe.name == "TransferSet" =>
        val xs: IndexedSeq[EVALUATED] = fields("transfers").asInstanceOf[ARR].xs
        val r = xs.map {
          case CaseObj(tpe, fields) if tpe.name == "Transfer" =>
            (fields("recipient"), fields("amount")) match {
              case (CaseObj(Types.addressType.typeRef, fields2), CONST_LONG(b)) =>
                fields2("bytes") match {
                  case CONST_BYTEVECTOR(addBytes) => (Address(addBytes), b)
                  case v                          => ???
                }
              case v => ???
            }
          case v => ???
        }
        Right(ContractResult(List.empty, r.toList))
    }
  }

}
