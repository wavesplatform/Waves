package com.wavesplatform.matcher.model

import cats.data.NonEmptyList
import com.wavesplatform.NoShrink
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.OrderBook.TickSize
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MatchingRulesSpecification extends PropSpec with PropertyChecks with Matchers with MatcherTestData with NoShrink {
  property("skipOutdated: rules.head.startOffset <= currentOffset < rules(1).startOffset") {
    val g = for {
      currOffset <- currOffsetGen
      rules      <- rulesChainGen(5)
    } yield (currOffset, rules)

    forAll(g) {
      case (currOffset, rules) =>
        val updatedRules = MatchingRules.skipOutdated(currOffset, rules)
        updatedRules.toList match {
          case first :: Nil =>
            withClue(s"first.startOffset=${first.startOffset}, currOffset=$currOffset") {
              first.startOffset shouldBe <=(currOffset)
            }
          case first :: second :: _ =>
            withClue(s"first.startOffset=${first.startOffset}, currOffset=$currOffset") {
              first.startOffset shouldBe <=(currOffset)
            }
            withClue(s"currOffset=$currOffset, second.startOffset=${second.startOffset}") {
              currOffset shouldBe <(second.startOffset)
            }
          case xs => throw new IllegalStateException(s"$xs")
        }
    }
  }

  private val currOffsetGen = Gen.choose(0L, Long.MaxValue)

  private def nextRulesGen(prevRules: MatchingRules): Gen[Option[MatchingRules]] =
    if (prevRules.startOffset == Long.MaxValue) Gen.const(None)
    else
      for {
        startOffset <- Gen.choose(prevRules.startOffset + 1, Long.MaxValue)
        disabled    <- Arbitrary.arbBool.arbitrary
        tickSize    <- if (disabled) Gen.const(TickSize.Disabled) else Gen.choose(1, Long.MaxValue).map(TickSize.Enabled)
      } yield Some(MatchingRules(startOffset, tickSize))

  private val firstRuleGen: Gen[MatchingRules] = for {
    disabled <- Arbitrary.arbBool.arbitrary
    tickSize <- if (disabled) Gen.const(TickSize.Disabled) else Gen.choose(1, Long.MaxValue).map(TickSize.Enabled)
  } yield MatchingRules(0L, tickSize)

  private def rulesChainGen(maxNumber: Int): Gen[NonEmptyList[MatchingRules]] = {
    def loop(rest: Int, acc: Gen[NonEmptyList[MatchingRules]]): Gen[NonEmptyList[MatchingRules]] =
      if (rest == 0) acc
      else
        for {
          xs <- acc
          x  <- nextRulesGen(xs.head)
          r  <- x.fold(Gen.const(xs))(x => loop(rest - 1, Gen.const(x :: xs)))
        } yield r

    Gen.lzy(loop(maxNumber, firstRuleGen.map(NonEmptyList.one)).map(_.reverse))
  }
}
