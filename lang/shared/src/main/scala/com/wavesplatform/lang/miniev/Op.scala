package com.wavesplatform.lang.miniev

import com.wavesplatform.lang.miniev.Ev.Scope
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.{ExecutionError, FailOrRejectError}

class LazyVal(var value: Either[(EXPR, Scope), EVALUATED]) {
  override def toString: String = value.fold(
    { case (expr, _) => s"N:$expr" },
    ev => s"E:$ev"
  )
}

trait Op {
  def ret(ev: EVALUATED): Op.Result
}

object Op {
  type Result = (Either[ExecutionError, EXPR], Option[Scope])

  def failOrReject(str: String): Result = Left(FailOrRejectError(str)) -> None

  case class Get(field: String) extends Op {
    override def ret(ev: EVALUATED): Result = ev match {
      case CaseObj(_, fields) =>
        fields.get(field) match {
          case Some(v) => Right(v) -> None
          case None    => failOrReject(s"object $ev has no field $field")
        }
      case other => failOrReject(s"$other is not an object")
    }
  }

  case class If(ifTrue: EXPR, ifFalse: EXPR) extends Op {
    override def ret(ev: EVALUATED): Result = ev match {
      case CONST_BOOLEAN(cond) => Right(if (cond) ifTrue else ifFalse) -> None
      case _                   => failOrReject(s"$ev is not a Boolean")
    }
  }

  case class FuncArg(func: FunctionHeader, reversedEvaluatedArgs: List[EVALUATED], argsToEvaluate: List[EXPR]) extends Op {
    override def ret(ev: EVALUATED): Result =
      Right(FUNCTION_CALL(func, (ev :: reversedEvaluatedArgs).foldLeft(argsToEvaluate) { case (a, v) => v :: a })) -> None
  }

  case class Value(key: String, lazyVal: LazyVal, state: State) extends Op {
    private val cachedScope = state.currentScope()

    override def ret(ev: EVALUATED): Result = {
      lazyVal.value = Right(ev)
      state.log(key, Right(ev))
      Right(ev) -> Some(cachedScope)
    }
  }

  case class Func(name: String, scope: Scope, predefinedComplexity: Option[Long] = None) extends Op {
    override def ret(ev: EVALUATED): Result = (Right(ev), Some(scope))
  }
}
