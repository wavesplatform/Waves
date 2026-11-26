package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.database.{AddressId, DBResource, Keys, RDB, findBalanceAtHeight, readCurrentBalance}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

import scala.annotation.tailrec

object BalanceDistribution {
  class BalanceIterator(
      resource: DBResource,
      globalPrefix: Array[Byte],
      asset: Asset,
      height: Int,
      private var pendingPortfolios: Map[(Address, Asset), Long],
      cfh: RDB.ByHeightHandle
  ) extends AbstractIterator[(Address, Long)] {
    @tailrec
    private def findNextBalance(): Option[(Address, Long)] = {
      val iterator = resource.fullIterator
      if (!iterator.isValid) None
      else {
        val key = iterator.key()
        if (!key.startsWith(globalPrefix)) None
        else {
          val aid = AddressId.fromByteArray(key.takeRight(Longs.BYTES))
          val balance = asset match {
            case Waves =>
              findBalanceAtHeight(readCurrentBalance(iterator.value()), height, h => resource.getOpt(Keys.wavesBalanceAt(aid, h, cfh)))
            case issued: IssuedAsset =>
              findBalanceAtHeight(
                resource.get(Keys.assetBalance(aid, issued)),
                height,
                h => resource.getOpt(Keys.assetBalanceAt(aid, issued, h, cfh))
              )
          }
          iterator.next()

          balance match {
            case Some((_, persisted)) =>
              val address  = resource.get(Keys.idToAddress(aid))
              val adjusted = pendingPortfolios.getOrElse((address, asset), persisted)
              pendingPortfolios -= ((address, asset))
              if (adjusted > 0) Some(address -> adjusted) else findNextBalance()
            case None => findNextBalance()
          }
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
