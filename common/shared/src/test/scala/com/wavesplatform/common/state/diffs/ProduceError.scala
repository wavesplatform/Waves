package com.wavesplatform.common.state.diffs

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right}

class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
  override def apply(ei: Either[_, _]): MatchResult = {
    ei match {
      case r @ Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
      case l @ Left(_) =>
        MatchResult(matches = l.toString contains errorMessage,
                    "expecting Left(...{0}...) but got {1}",
                    "got expected error",
                    IndexedSeq(errorMessage, l))
    }
  }
}

object ProduceError {
  def produce(err: String): Matcher[Either[_, _]] = new ProduceError(err)
}
