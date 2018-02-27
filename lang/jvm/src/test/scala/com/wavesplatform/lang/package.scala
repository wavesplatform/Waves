package com.wavesplatform

import com.wavesplatform.lang.Context.PredefFunction

import scala.util.Try

package object lang {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: PredefFunction = PredefFunction("MULTIPLY", Terms.INT, List(("x1", Terms.INT), ("x2", Terms.INT))) {
    case (x1: Int) :: (x2: Int) :: Nil => Try(x1 * x2).toEither.left.map(_.toString)
    case _                             => ??? // suppress pattern match warning
  }
}
