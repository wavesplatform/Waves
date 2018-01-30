package com.wavesplatform

import org.scalacheck.Shrink
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right}


package object lang {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  trait NoShrink {
    implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  }

  class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
    override def apply(ei: Either[_, _]): MatchResult = {
      ei match {
        case r@Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
        case l@Left(_) => MatchResult(matches = l.toString contains errorMessage,
          "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, l))
      }
    }
  }

}
