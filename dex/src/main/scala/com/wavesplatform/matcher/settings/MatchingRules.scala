package com.wavesplatform.matcher.settings

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import cats.implicits._
import com.wavesplatform.matcher.model.OrderBook.TickSize
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

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

case class RawMatchingRules(startOffset: Long, mergePrices: Boolean, tickSize: Double = 0)

object RawMatchingRules {

  private implicit val rawMatchingRulesReader: ValueReader[RawMatchingRules] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    val offsetValidated      = cfgValidator.validateByPredicate[Long](s"$path.start-offset")(_ >= 0, "required 0 <= start offset")
    val mergePricesValidated = cfgValidator.validate[Boolean](s"$path.merge-prices")

    val tickSizeValidated = mergePricesValidated match {
      case Valid(true) => cfgValidator.validateByPredicate[Double](s"$path.tick-size")(_ > 0, "required 0 < tick size")
      case _           => Valid(0d)
    }

    (
      offsetValidated,
      mergePricesValidated,
      tickSizeValidated
    ) mapN RawMatchingRules.apply getValueOrThrowErrors
  }

  implicit val rawMatchingRulesNelReader: ValueReader[NonEmptyList[RawMatchingRules]] = {
    com.wavesplatform.settings.nonEmptyListReader[RawMatchingRules].map { xs =>
      val isStrictOrder = xs.tail.zip(xs.toList).forall { case (next, prev) => next.startOffset > prev.startOffset }
      if (isStrictOrder) { if (xs.head.startOffset != 0) RawMatchingRules(0, false) :: xs else xs } else
        throw new IllegalArgumentException(s"Rules should be ordered by offset, but they are: ${xs.map(_.startOffset).toList.mkString(", ")}")
    }
  }
}
