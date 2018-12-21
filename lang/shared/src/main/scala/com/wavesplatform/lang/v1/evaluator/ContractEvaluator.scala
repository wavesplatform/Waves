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
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient, Tx}
import scodec.bits.ByteVector
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.traits.domain.Tx.Pmt

import scala.collection.mutable.ListBuffer

object ContractEvaluator {
  case class Invokation(name: String,
                        fc: FUNCTION_CALL,
                        invoker: ByteVector,
                        payment: Option[(Long, Option[ByteVector])],
                        contractAddress: ByteVector)

  def eval(c: Contract, i: Invokation): EvalM[EVALUATED] = {
    c.cfs.find(_.u.name == i.name) match {
      case None => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"Callable function '${i.name} doesn't exist in the contract")
      case Some(f) =>
        val zeroExpr = (f.p, i.payment) match {
          case (None, None) =>
            Right(
              BLOCKV2(
                LET(f.c.pubKeyArgName, CONST_BYTEVECTOR(i.invoker)),
                BLOCKV2(
                  LET("invocation",
                      Bindings.buildInvocation(Recipient.Address(i.invoker),
                                               i.payment.map { case (a, t) => Pmt(t, a) },
                                               Recipient.Address(i.contractAddress))),
                  BLOCKV2(f.u, i.fc)
                )
              ))
          case (Some(pf), Some((amt, token))) =>
            Right(
              BLOCKV2(
                LET(f.c.pubKeyArgName, CONST_BYTEVECTOR(i.invoker)),
                BLOCKV2(
                  LET(pf.amountArgName, CONST_LONG(amt)),
                  BLOCKV2(
                    LET(pf.tokenArgName, token match {
                      case None    => com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
                      case Some(t) => CONST_BYTEVECTOR(t)
                    }),
                    BLOCKV2(f.u, i.fc)
                  )
                )
              ))
          case _ => Left("Bad payable")
        }

        for {
          ze <- liftEither(zeroExpr)
          expr = c.dec.foldRight(ze)((d, e) => BLOCKV2(d, e))
          r <- EvaluatorV1.evalExpr(expr)
        } yield r
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

case class ContractResult(ds: List[DataItem[_]], ts: List[(Address, Long, Option[ByteVector])])
object ContractResult {
  private def processWriteSet(c: CaseObj) = c match {
    case CaseObj(_, fields) =>
      val xs: IndexedSeq[EVALUATED] = fields("data").asInstanceOf[ARR].xs
      xs.map {
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
  }

  private def processTransferSet(c: CaseObj) = c match {
    case CaseObj(tpe, fields) =>
      val xs: IndexedSeq[EVALUATED] = fields("transfers").asInstanceOf[ARR].xs
      xs.map {
        case CaseObj(_, fields) if tpe.name == "Transfer" =>
          (fields("recipient"), fields("amount"), fields("asset")) match {
            case (CaseObj(Types.addressType.typeRef, fields2), CONST_LONG(b), t) =>
              val token = t match {
                case CONST_BYTEVECTOR(tokenId)  => Some(tokenId)
                case CaseObj(_, m) if m.isEmpty => None
                case _                          => ???
              }

              fields2("bytes") match {
                case CONST_BYTEVECTOR(addBytes) => (Address(addBytes), b, token)
                case v                          => ???
              }
            case v => ???
          }
        case v => ???
      }
  }

  private def processContractSet(c: CaseObj) = c match {
    case CaseObj(_, fields) =>
      val writes = fields("data") match {
        case c @ CaseObj(tpe, _) if tpe.name == "WriteSet" => processWriteSet(c)
        case _                                             => ???
      }
      val payments = fields("payments") match {
        case c @ CaseObj(tpe, _) if tpe.name == "WriteSet" => processTransferSet(c)
        case _                                             => ???
      }
      ContractResult(writes.toList, payments.toList)
  }

  def fromObj(e: EVALUATED): Either[ExecutionError, ContractResult] = Right {
    e match {
      case c @ CaseObj(tpe, _) =>
        tpe.name match {
          case "WriteSet"       => ContractResult(processWriteSet(c).toList, List.empty)
          case "TransferSet"    => ContractResult(List.empty, processTransferSet(c).toList)
          case "ContractResult" => processContractSet(c)
          case _                => ???
        }
      case _ => ???
    }
  }
}
