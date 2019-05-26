package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.KeyPair
import com.wavesplatform.transaction.{Asset, Proofs}

class OrderOps(val o: Order) extends AnyVal {
  @inline def copy(withV1: OrderV1 => OrderV1, withV2: OrderV2 => OrderV2, withV3: OrderV3 => OrderV3): Order = {
    o match {
      case o1: OrderV1 => withV1(o1)
      case o2: OrderV2 => withV2(o2)
      case o3: OrderV3 => withV3(o3)
    }
  }

  @inline def updateProofs(p: Proofs): Order = {
    copy(
      _.copy(proofs = p),
      _.copy(proofs = p),
      _.copy(proofs = p)
    )
  }

  @inline def updateExpiration(expiration: Long): Order = {
    copy(
      _.copy(expiration = expiration),
      _.copy(expiration = expiration),
      _.copy(expiration = expiration)
    )
  }
  @inline def updateTimestamp(timestamp: Long): Order = {
    copy(
      _.copy(timestamp = timestamp),
      _.copy(timestamp = timestamp),
      _.copy(timestamp = timestamp)
    )
  }
  @inline def updateFee(fee: Long): Order = {
    copy(
      _.copy(matcherFee = fee),
      _.copy(matcherFee = fee),
      _.copy(matcherFee = fee)
    )
  }
  @inline def updateAmount(amount: Long): Order = {
    copy(
      _.copy(amount = amount),
      _.copy(amount = amount),
      _.copy(amount = amount)
    )
  }
  @inline def updatePrice(price: Long): Order = {
    copy(
      _.copy(price = price),
      _.copy(price = price),
      _.copy(price = price)
    )
  }
  @inline def updateMatcher(pk: KeyPair): Order = {
    copy(
      _.copy(matcherPublicKey = pk),
      _.copy(matcherPublicKey = pk),
      _.copy(matcherPublicKey = pk)
    )
  }
  @inline def updateSender(pk: KeyPair): Order = {
    copy(
      _.copy(senderPublicKey = pk),
      _.copy(senderPublicKey = pk),
      _.copy(senderPublicKey = pk)
    )
  }
  @inline def updatePair(pair: AssetPair): Order = {
    copy(
      _.copy(assetPair = pair),
      _.copy(assetPair = pair),
      _.copy(assetPair = pair)
    )
  }
  @inline def updateType(t: OrderType): Order = {
    copy(
      _.copy(orderType = t),
      _.copy(orderType = t),
      _.copy(orderType = t)
    )
  }
  @inline def updateMatcherFeeAssetId(a: Asset): Order = {
    copy(
      identity,
      identity,
      _.copy(matcherFeeAssetId = a)
    )
  }
}

object OrderOps {
  implicit def toOps(o: Order): OrderOps = new OrderOps(o)
}
