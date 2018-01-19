package com.wavesplatform.it.matcher

import com.wavesplatform.it.Node
import com.wavesplatform.it.matcher.OrderExclusionTestSuite.MatcherFee
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._


trait OrderGenerator {

  def prepareOrder(node: Node, matcherNode: Node, pair: AssetPair, orderType: OrderType, price: Long, amount: Long,
                   timeToLive: Duration = 30.days - 1.seconds): Order = {
    val creationTime = System.currentTimeMillis()
    val timeToLiveTimestamp = creationTime + timeToLive.toMillis

    val privateKey = node.privateKey
    val matcherPublicKey = matcherNode.publicKey

    Order(privateKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, MatcherFee)
  }


}
