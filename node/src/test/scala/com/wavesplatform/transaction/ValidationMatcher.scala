package com.wavesplatform.transaction

import com.wavesplatform.transaction.assets.exchange.Validation
import org.scalatest.enablers.Containing
import org.scalatest.matchers.{BeMatcher, MatchResult}

trait ValidationMatcher {
  class ValidationMatcher extends BeMatcher[Validation] {
    def apply(left: Validation): MatchResult =
      MatchResult(
        left.status,
        left.toString + " was invalid",
        left.toString + " was valid"
      )
  }

  val valid = new ValidationMatcher

  implicit val containingNatureOfValidation: Containing[Validation] =
    new Containing[Validation] {
      def contains(v: Validation, ele: Any): Boolean =
        !v.status && v.labels.contains(ele.toString)

      def containsOneOf(v: Validation, elements: scala.collection.Seq[Any]): Boolean = {
        !v.status && elements.map(_.toString).map(v.labels.contains).reduce(_ || _)
      }

      def containsNoneOf(v: Validation, elements: scala.collection.Seq[Any]): Boolean = {
        v.status || elements.map(_.toString).map(v.labels.contains).reduce(!_ && !_)
      }
    }

}
