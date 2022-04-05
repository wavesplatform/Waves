package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.database.{AddressId, DBResource, Keys}
import com.wavesplatform.state.{Portfolio, safeSum}

import scala.annotation.tailrec

object BalanceDistribution {
  class BalanceIterator(
      resource: DBResource,
      globalPrefix: Array[Byte],
      addressId: Array[Byte] => AddressId,
      balanceOf: Portfolio => Long,
      height: Int,
      private var pendingPortfolios: Map[Address, Portfolio]
  ) extends AbstractIterator[(Address, Long)] {
    @inline
    private def stillSameAddress(expected: AddressId): Boolean = resource.iterator.hasNext && {
      val maybeNext = resource.iterator.peekNext().getKey
      maybeNext.startsWith(globalPrefix) && addressId(maybeNext) == expected
    }
    @tailrec
    private def findNextBalance(): Option[(Address, Long)] = {
      if (!resource.iterator.hasNext) None
      else {
        val current = resource.iterator.next()
        if (!current.getKey.startsWith(globalPrefix)) None
        else {
          val aid           = addressId(current.getKey)
          val address       = resource.get(Keys.idToAddress(aid))
          var balance       = Longs.fromByteArray(current.getValue)
          var currentHeight = Ints.fromByteArray(current.getKey.takeRight(4))

          while (stillSameAddress(aid)) {
            val next       = resource.iterator.next()
            val nextHeight = Ints.fromByteArray(next.getKey.takeRight(4))
            if (nextHeight <= height) {
              currentHeight = nextHeight
              balance = Longs.fromByteArray(next.getValue)
            }
          }

          val adjustedBalanceE = safeSum(balance, pendingPortfolios.get(address).fold(0L)(balanceOf), "Next distribution balance")
          pendingPortfolios -= address

          adjustedBalanceE match {
            case Right(adjustedBalance) if currentHeight <= height && adjustedBalance > 0 => Some(address -> adjustedBalance)
            case _                                                                        => findNextBalance()
          }
        }
      }
    }

    override def computeNext(): (Address, Long) = findNextBalance() match {
      case Some(balance) => balance
      case None =>
        if (pendingPortfolios.nonEmpty) {
          val (address, portfolio) = pendingPortfolios.head
          pendingPortfolios -= address
          address -> balanceOf(portfolio)
        } else {
          endOfData()
        }
    }
  }
}
