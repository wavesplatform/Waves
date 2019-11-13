package com.wavesplatform.transaction.assets

import com.wavesplatform.account.KeyPair
import com.wavesplatform.transaction.{Asset, Proofs}

package object exchange {
  implicit class OrderOps(private val o: Order) extends AnyVal {
    @inline def updateProofs(p: Proofs): Order = {
      o.copy(proofs = p)
    }
    @inline def updateExpiration(expiration: Long): Order = {
      o.copy(expiration = expiration)
    }
    @inline def updateTimestamp(timestamp: Long): Order = {
      o.copy(timestamp = timestamp)
    }
    @inline def updateFee(fee: Long): Order = {
      o.copy(matcherFee = fee)
    }
    @inline def updateAmount(amount: Long): Order = {
      o.copy(amount = amount)
    }
    @inline def updatePrice(price: Long): Order = {
      o.copy(price = price)
    }
    @inline def updateMatcher(pk: KeyPair): Order = {
      o.copy(matcherPublicKey = pk)
    }
    @inline def updateSender(pk: KeyPair): Order = {
      o.copy(senderPublicKey = pk)
    }
    @inline def updatePair(pair: AssetPair): Order = {
      o.copy(assetPair = pair)
    }
    @inline def updateType(t: OrderType): Order = {
      o.copy(orderType = t)
    }
    @inline def updateMatcherFeeAssetId(a: Asset): Order = {
      o.copy(matcherFeeAssetId = a)
    }
  }
}
