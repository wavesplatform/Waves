package com.wavesplatform.matcher
import com.wavesplatform.matcher.model.{LevelAgg, LimitOrder}
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}

package object api {
  def aggregateLevel(l: (Price, Level[LimitOrder])) = LevelAgg(l._2.view.map(_.amount).sum, l._1)
}
