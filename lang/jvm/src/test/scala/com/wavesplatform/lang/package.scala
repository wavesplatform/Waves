package com.wavesplatform

import com.wavesplatform.lang.Context.CustomFunction

import scala.util.Try

package object lang {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: CustomFunction = CustomFunction("MULTIPLY", Terms.INT, List(("x1", Terms.INT), ("x2", Terms.INT))) {
    case x1 :: x2 :: Nil => Try { x1.asInstanceOf[Int] * x2.asInstanceOf[Int] }.toEither.left.map(_.toString)
    case _               => ??? // suppress pattern match warning
  }
}
