package com.wavesplatform.lang.miniev

import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.{Failure, Success, Try}

trait ComplexityCounter {
  def recordLet(): Try[Unit]
  def recordFunctionArguments(argCount: Int): Try[Unit]
  def recordComplexity(complexity: Int): Try[Unit]
  def recordGet(): Try[Unit]
  def spentComplexity: Int
}

object ComplexityCounter {
  private val success = Success(())
  private val failure = Failure(new ArithmeticException with NoStackTrace)



  class New extends ComplexityCounter {
    private var totalSpentComplexity = 0

    override def recordLet(): Try[Unit] = success

    override def recordGet(): Try[Unit] = success

    override def recordFunctionArguments(argCount: Int): Try[Unit] = success
    override def recordComplexity(complexity: Int): Try[Unit] =
      try {
        totalSpentComplexity = math.addExact(totalSpentComplexity, complexity)
        success
      } catch {
        case NonFatal(_) => failure
      }

    override def spentComplexity: Int = totalSpentComplexity
  }
}
