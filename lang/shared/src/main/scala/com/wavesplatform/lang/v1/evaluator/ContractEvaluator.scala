package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.VerifierFunction
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Bindings, FieldNames, Types}
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports.{raiseError, _}
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.Tx.Pmt
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient, Tx}

import scala.collection.mutable.ListBuffer

object ContractEvaluator {
  case class Invokation(fc: FUNCTION_CALL, invoker: ByteStr, payment: Option[(Long, Option[ByteStr])], contractAddress: ByteStr)

  def eval(c: Contract, i: Invokation): EvalM[EVALUATED] = {
    val functionName = i.fc.function.asInstanceOf[FunctionHeader.User].name
    c.cfs.find(_.u.name == functionName) match {
      case None => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"Callable function '$functionName doesn't exist in the contract")
      case Some(f) =>
        val zeroExpr = Right(
          BLOCK(
            LET(f.annotation.invocationArgName,
                Bindings
                  .buildInvocation(Recipient.Address(i.invoker), i.payment.map { case (a, t) => Pmt(t, a) }, Recipient.Address(i.contractAddress))),
            BLOCK(f.u, i.fc)
          ))

        for {
          ze <- liftEither(zeroExpr)
          expr = c.dec.foldRight(ze)((d, e) => BLOCK(d, e))
          r <- EvaluatorV1.evalExpr(expr)
        } yield r
    }
  }

  def verify(v: VerifierFunction, tx: Tx): EvalM[EVALUATED] = {
    val t = Bindings.transactionObject(tx, proofsEnabled = true)
    val expr =
      BLOCK(LET(v.annotation.txArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
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

case class ContractResult(ds: List[DataItem[_]], ts: List[(Address, Long, Option[ByteStr])])
object ContractResult {
  private def processWriteSet(c: CaseObj) = c match {
    case CaseObj(_, fields) =>
      val xs: IndexedSeq[EVALUATED] = fields(FieldNames.Data).asInstanceOf[ARR].xs
      xs.map {
        case CaseObj(tpe, fields) if tpe.name == "DataEntry" =>
          (fields("key"), fields("value")) match {
            case (CONST_STRING(k), CONST_BOOLEAN(b)) => DataItem.Bool(k, b)
            case (CONST_STRING(k), CONST_STRING(b))  => DataItem.Str(k, b)
            case (CONST_STRING(k), CONST_LONG(b))    => DataItem.Lng(k, b)
            case (CONST_STRING(k), CONST_BYTESTR(b)) => DataItem.Bin(k, b)
            case _                                   => ???
          }
        case _ => ???
      }
  }

  private def processTransferSet(c: CaseObj) = c match {
    case CaseObj(tpe, fields) =>
      val xs: IndexedSeq[EVALUATED] = fields(FieldNames.Transfers).asInstanceOf[ARR].xs
      xs.map {
        case CaseObj(t, fields) if t.name == FieldNames.ContractTransfer =>
          (fields("recipient"), fields("amount"), fields("asset")) match {
            case (CaseObj(Types.addressType.typeRef, fields2), CONST_LONG(b), t) =>
              val token = t match {
                case CONST_BYTESTR(tokenId)     => Some(tokenId)
                case CaseObj(_, m) if m.isEmpty => None
                case _                          => ???
              }

              fields2("bytes") match {
                case CONST_BYTESTR(addBytes) => (Address(addBytes), b, token)
                case v                       => ???
              }
            case v => ???
          }
        case _ => ???
      }
  }

  private def processContractSet(c: CaseObj) = c match {
    case CaseObj(_, fields) =>
      val writes = fields(FieldNames.Data) match {
        case c @ CaseObj(tpe, _) if tpe.name == FieldNames.WriteSet => processWriteSet(c)
        case _                                                      => ???
      }
      val payments = fields(FieldNames.Transfers) match {
        case c @ CaseObj(tpe, _) if tpe.name == FieldNames.TransferSet => processTransferSet(c)
        case _                                                         => ???
      }
      ContractResult(writes.toList, payments.toList)
  }

  def fromObj(e: EVALUATED): Either[ExecutionError, ContractResult] = Right {
    e match {
      case c @ CaseObj(tpe, _) =>
        tpe.name match {
          case FieldNames.WriteSet       => ContractResult(processWriteSet(c).toList, List.empty)
          case FieldNames.TransferSet    => ContractResult(List.empty, processTransferSet(c).toList)
          case FieldNames.ContractResult => processContractSet(c)
          case _                         => ???
        }
      case _ => ???
    }
  }
}
