package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports.raiseError
import com.wavesplatform.lang.v1.traits.domain.DataItem
import scodec.bits.ByteVector

object ContractEvaluator {
  case class Invokation(name: String, fc: FUNCTION_CALL, invoker: ByteVector)

  def eval(c: Contract, i: Invokation): EvalM[EVALUATED] = {
    c.cfs.find(_.u.name == i.name) match {
      case None => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"Callable function '${i.name} doesn't exist in the contract")
      case Some(f) =>
        val expr =
          BLOCKV2(
            LET(f.c.pubKeyArgName, CONST_BYTEVECTOR(i.invoker)),
            BLOCKV2(f.u, i.fc)
          )
        EvaluatorV1.evalExpr(expr)
    }
  }

  case class WriteSet(l: List[DataItem[_]])
  object WriteSet {
    def fromObj(e: EVALUATED): Either[ExecutionError, WriteSet] = {
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
              }
            case _ => ???
          }
          Right(WriteSet(r.toList))
        case t => Left(s"Unexpected exec result $t")
      }
    }
  }

  def apply(ctx: EvaluationContext, c: Contract, i: Invokation): Either[ExecutionError, WriteSet] = {
    val lec = LoggedEvaluationContext(_ => _ => (), ctx)
    eval(c, i).run(lec).value._2.flatMap(WriteSet.fromObj)

  }
}
