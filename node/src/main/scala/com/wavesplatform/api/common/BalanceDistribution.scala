package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.database.{AddressId, DBResource, Keys}
import com.wavesplatform.transaction.Asset

import scala.annotation.tailrec

object BalanceDistribution {
  class BalanceIterator(
      resource: DBResource,
      globalPrefix: Array[Byte],
      addressId: Array[Byte] => AddressId,
      asset: Asset,
      height: Int,
      private var pendingPortfolios: Map[(Address, Asset), Long]
  ) extends AbstractIterator[(Address, Long)] {
    @inline
    private def stillSameAddress(expected: AddressId): Boolean = resource.fullIterator.isValid && {
      val maybeNext = resource.fullIterator.key()
      maybeNext.startsWith(globalPrefix) && addressId(maybeNext) == expected
    }
    @tailrec
    private def findNextBalance(): Option[(Address, Long)] = {
      if (!resource.fullIterator.isValid) None
      else {
        val key   = resource.fullIterator.key()
        val value = resource.fullIterator.value()
        if (!key.startsWith(globalPrefix)) None
        else {
          val aid           = addressId(key)
          val address       = resource.get(Keys.idToAddress(aid))
          var balance       = Longs.fromByteArray(value)
          var currentHeight = Ints.fromByteArray(key.takeRight(4))

          while (stillSameAddress(aid)) {
            val nextHeight = Ints.fromByteArray(resource.fullIterator.key.takeRight(4))
            if (nextHeight <= height) {
              currentHeight = nextHeight
              balance = Longs.fromByteArray(resource.fullIterator.value())
            }
            resource.fullIterator.next()
          }

          val adjustedBalance = pendingPortfolios.getOrElse((address, asset), balance)
          pendingPortfolios = pendingPortfolios.removed((address, asset))

          if (currentHeight <= height && adjustedBalance > 0)
            Some(address -> adjustedBalance)
          else
            findNextBalance()
        }
      }
    }

    override def computeNext(): (Address, Long) = findNextBalance() match {
      case Some(balance) => balance
      case None =>
        pendingPortfolios.headOption
          .collect { case (key @ (address, `asset`), balance) =>
            pendingPortfolios -= key
            address -> balance
          }
          .getOrElse(endOfData())
    }
  }
}
