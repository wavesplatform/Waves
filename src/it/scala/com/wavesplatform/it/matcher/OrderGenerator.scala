package com.wavesplatform.it.matcher

import com.wavesplatform.it.Node
import com.wavesplatform.it.matcher.OrderExclusionTestSuite.MatcherFee
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scala.concurrent.duration._


trait OrderGenerator {

  def prepareOrder(node: Node, matcherNode: Node, pair: AssetPair, orderType: OrderType, price: Long, amount: Long,
                   timeToLive: Duration = 30.days - 1.seconds): Order = {
    val creationTime = System.currentTimeMillis()
    val timeToLiveTimestamp = creationTime + timeToLive.toMillis

    val privateKey = PrivateKeyAccount.fromSeed(node.accountSeed).right.get
    val matcherPublicKey = PublicKeyAccount(Base58.decode(matcherNode.publicKey).get)

    Order(privateKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, MatcherFee)
  }


}
