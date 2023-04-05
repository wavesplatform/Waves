package com.wavesplatform.lang.miniev

import com.wavesplatform.lang.{CommonError, ExecutionError, SoftLimitReached}

sealed trait ComplexityLimit {
  def checkLimit(spentComplexity: Long): Either[ExecutionError, Long]
}

object ComplexityLimit {
  sealed abstract class Limit(maxComplexity: Long, error: => ExecutionError) extends ComplexityLimit {
    override def checkLimit(spentComplexity: Long): Either[ExecutionError, Long] =
      Either.cond(spentComplexity <= maxComplexity, spentComplexity, error)
  }

  case class Partial(limit: Int) extends Limit(limit, SoftLimitReached)
  case class Complete(limit: Int) extends Limit(limit, CommonError(s"Complexity limit $limit reached"))

  case object Unlimited extends ComplexityLimit {
    override def checkLimit(spentComplexity: Long): Either[ExecutionError, Long] = Right(spentComplexity)
  }
}
