package com.wavesplatform.it.sync

import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state.ByteStr
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._

package object matcher {
  def prepareOrder(node: Node,
                   matcherNode: Node,
                   pair: AssetPair,
                   orderType: OrderType,
                   price: Long,
                   amount: Long,
                   timeToLive: Duration = 30.days - 1.seconds): Order = {
    val creationTime        = System.currentTimeMillis()
    val timeToLiveTimestamp = creationTime + timeToLive.toMillis
    val matcherPublicKey    = matcherNode.publicKey
    val unsigned            = Order(node.publicKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, 300000, Array())
    val signature           = crypto.sign(node.privateKey, unsigned.toSign)
    unsigned.copy(signature = signature)
  }

  def getOrderBook(node: Node, matcherNode: Node) = {
    val ts         = System.currentTimeMillis()
    val privateKey = node.privateKey
    val publicKey  = node.publicKey.publicKey
    val signature  = ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(ts)))

    matcherNode.getOrderbookByPublicKey(node.publicKeyStr, ts, signature)
  }

  def getActiveOrderBook(node: Node, matcherNode: Node) = {
    val ts         = System.currentTimeMillis()
    val privateKey = node.privateKey
    val publicKey  = node.publicKey.publicKey
    val signature  = ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(ts)))

    matcherNode.getOrderbookByPublicKeyActive(node.publicKeyStr, ts, signature)
  }

  def getReservedBalance(node: Node, matcherNode: Node) = {
    val ts         = System.currentTimeMillis()
    val privateKey = node.privateKey
    val publicKey  = node.publicKey.publicKey
    val signature  = ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(ts)))

    matcherNode.getReservedBalance(node.publicKeyStr, ts, signature)
  }

  def matcherCancelOrder(node: Node, matcherNode: Node, pair: AssetPair, orderId: String) = {
    val privateKey    = node.privateKey
    val publicKey     = node.publicKey
    val request       = CancelOrderRequest(publicKey, Some(orderId), Array.emptyByteArray, None)
    val sig           = crypto.sign(privateKey, request.toSign)
    val signedRequest = request.copy(signature = sig)
    matcherNode.cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)
  }

}
