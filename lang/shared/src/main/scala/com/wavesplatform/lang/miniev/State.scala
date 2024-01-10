package com.wavesplatform.lang.miniev

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.miniev.Ev.{Closure, Scope}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.LetExecResult
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.{CommonError, ExecutionError}

abstract class State(complexityLimit: ComplexityLimit, val newMode: Boolean) {
  private var stack: List[Op]             = Nil
  private var complexityStack: List[Long] = Nil
  private var complexityCounter           = 0L
  private var scope                       = Scope(Map.empty, Map.empty)

  var logEntries = Vector.empty[(String, LetExecResult[Id])]

  def evaluationContext: EvaluationContext[Id]
  def stdlibVersion: StdLibVersion

  def log(name: String, value: LetExecResult[Id]): this.type = {
    logEntries = logEntries :+ (name, value)
    this
  }

  def recordComplexityOverhead(): Either[ExecutionError, Long] = spendComplexity(if (newMode) 0 else 1)

  def spendComplexity(c: Long): Either[ExecutionError, Long] =
    try {
      complexityCounter = Math.addExact(complexityCounter, c)
      totalSpentComplexity().flatMap(tsc => complexityLimit.checkLimit(tsc))
    } catch { case _: ArithmeticException => CommonError("Complexity overflow").asLeft }

  def totalSpentComplexity(): Either[ExecutionError, Long] =
    try { complexityStack.foldLeft(complexityCounter)(Math.addExact).asRight }
    catch { case _: ArithmeticException => CommonError("Complexity overflow").asLeft }

  def popComplexity(): Long = {
    val tmp = complexityCounter
    if (complexityStack.isEmpty) {
      complexityCounter = 0
    } else {
      complexityCounter = complexityStack.head
      complexityStack = complexityStack.tail
    }
    tmp
  }

  def push(op: Op): this.type = {
    stack = op :: stack
    this
  }

  def pop(): Option[Op] = {
    val op = stack.headOption
    stack = if (stack.nonEmpty) stack.tail else Nil
    op
  }

  def callUserFunction(name: String, functionScope: Scope, predefinedComplexity: Option[Long]): this.type = {
    stack ::= Op.Func(name, currentScope(), predefinedComplexity)
    scope = functionScope
    complexityStack ::= complexityCounter
    complexityCounter = 0L
    this
  }

  def addName(name: String, value: EXPR): this.type = {
    scope = scope.copy(names = scope.names + (name -> new LazyVal(Left((value, scope)))))
    this
  }

  def cachingEv(name: String): Option[LazyVal] = scope.names.get(name)

  def addUserFunction(func: FUNC): this.type = {
    scope = scope.copy(userFns = scope.userFns + (func.name -> Closure(func, scope)))
    this
  }

  def resetScope(newScope: Scope): this.type = {
    scope = newScope
    this
  }

  def currentScope(): Scope = scope
}

object State {
  def apply(ec: EvaluationContext[Id], complexityLimit: ComplexityLimit, newMode: Boolean, version: StdLibVersion): State =
    new State(complexityLimit, newMode) {
      override def stdlibVersion: StdLibVersion             = version
      override def evaluationContext: EvaluationContext[Id] = ec
    }
}
