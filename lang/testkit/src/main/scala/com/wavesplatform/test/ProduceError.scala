package com.wavesplatform.test

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right}

class ProduceError(errorMessage: String) extends Matcher[Either[?, ?]] {
  override def apply(ei: Either[?, ?]): MatchResult = {
    ei match {
      case r @ Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
      case l @ Left(_) =>
        MatchResult(
          matches = l.toString `contains` errorMessage,
          "expecting Left(...{0}...) but got {1}",
          "got expected error",
          IndexedSeq(errorMessage, l)
        )
    }
  }
}
