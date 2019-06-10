package com.wavesplatform.matcher.model

import cats.data.NonEmptyList
import com.wavesplatform.matcher.model.OrderBook.TickSize
import com.wavesplatform.matcher.queue.QueueEventWithMeta

case class MatchingRules(startOffset: QueueEventWithMeta.Offset, tickSize: TickSize)

object MatchingRules {
  val Default                                 = MatchingRules(0L, TickSize.Disabled)
  val DefaultNel: NonEmptyList[MatchingRules] = NonEmptyList.one(Default)

  def skipOutdated(currOffset: QueueEventWithMeta.Offset, rules: NonEmptyList[MatchingRules]): NonEmptyList[MatchingRules] =
    if (currOffset > rules.head.startOffset)
      rules.tail match {
        case x :: xs =>
          if (currOffset == x.startOffset) NonEmptyList(x, xs)
          else if (currOffset > x.startOffset) skipOutdated(currOffset, NonEmptyList(x, xs))
          else rules
        case _ => rules
      } else rules
}
