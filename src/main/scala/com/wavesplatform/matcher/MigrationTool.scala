package com.wavesplatform.matcher

import java.io.File
import java.util.{HashMap => JHashMap}

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.Shorts
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.crypto.DigestSize
import com.wavesplatform.database.DBExt
import com.wavesplatform.db.openDB
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.{LimitOrder, OrderInfo}
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

import scala.collection.JavaConverters._

object MigrationTool extends ScorexLogging {
  private def collectStats(db: DB): Unit = {
    log.info("Collecting stats")
    val iterator = db.iterator()
    iterator.seekToFirst()

    val result = new JHashMap[Short, Stats]

    def add(prefix: Short, e: java.util.Map.Entry[Array[Byte], Array[Byte]]): Unit = {
      result.compute(
        prefix,
        (_, maybePrev) =>
          maybePrev match {
            case null => Stats(1, e.getKey.length, e.getValue.length)
            case prev => Stats(prev.entryCount + 1, prev.totalKeySize + e.getKey.length, prev.totalValueSize + e.getValue.length)
        }
      )
    }

    try {
      while (iterator.hasNext) {
        val e = iterator.next()
        e.getKey match {
          case SK.Orders(_)                => add(100.toShort, e)
          case SK.OrdersInfo(_)            => add(101.toShort, e)
          case SK.AddressToOrders(_)       => add(102.toShort, e)
          case SK.AddressToActiveOrders(_) => add(103.toShort, e)
          case SK.AddressPortfolio(_)      => add(104.toShort, e)
          case SK.Transactions(_)          => add(104.toShort, e)
          case SK.OrdersToTxIds(_)         => add(106.toShort, e)
          case bytes =>
            val prefix = Shorts.fromByteArray(bytes.take(2))
            add(prefix, e)
        }
      }
    } finally iterator.close()

    for ((k, s) <- result.asScala) {
      println(s"$k, ${s.entryCount}, ${s.totalKeySize}, ${s.totalValueSize}")
    }
  }

  private def deleteLegacyEntries(db: DB): Unit = {
    val keysToDelete = Seq.newBuilder[Array[Byte]]

    db.iterateOver("matcher:".getBytes(UTF_8)) { e =>
      keysToDelete += e.getKey
    }

    db.readWrite(rw => keysToDelete.result().foreach(rw.delete))
  }

  private def recalculateReservedBalance(db: DB): Unit = {
    log.info("Recalculating reserved balances")
    val calculatedReservedBalances = new JHashMap[Address, Map[Option[AssetId], Long]]()
    val ordersToDelete             = Seq.newBuilder[Array[Byte]]
    val orderInfoToUpdate          = Seq.newBuilder[(Array[Byte], OrderInfo)]
    val key                        = MatcherKeys.orderInfo(Array.emptyByteArray)

    var discrepancyCounter = 0

    db.iterateOver(key.keyBytes) { e =>
      val orderId = new Array[Byte](DigestSize)
      Array.copy(e.getKey, 2, orderId, 0, DigestSize)
      val orderInfo = key.parse(e.getValue)
      if (!orderInfo.status.isFinal) {
        db.get(MatcherKeys.order(orderId)) match {
          case None =>
            log.info(s"Missing order $orderId")
            ordersToDelete += orderId
          case Some(order) =>
            calculatedReservedBalances.compute(
              order.sender, { (_, prevBalances) =>
                val spendId        = order.getSpendAssetId
                val spendRemaining = order.getSpendAmount(order.price, orderInfo.remaining).explicitGet()
                val remainingFee   = order.matcherFee - LimitOrder.getPartialFee(order.matcherFee, order.amount, orderInfo.filled)

                if (remainingFee != orderInfo.remainingFee) {
                  orderInfoToUpdate += orderId -> orderInfo.copy(remainingFee = remainingFee)
                }

                val r = Option(prevBalances).fold(Map(spendId -> spendRemaining)) { prevBalances =>
                  prevBalances.updated(spendId, prevBalances.getOrElse(spendId, 0L) + spendRemaining)
                }

                // Fee correction
                if (order.getReceiveAssetId.isEmpty) r else r.updated(None, r.getOrElse(None, 0L) + remainingFee)
              }
            )
        }
      }
    }

    log.info("Collecting all addresses")

    val addresses = Seq.newBuilder[Address]
    db.iterateOver(Shorts.toByteArray(5)) { e =>
      val addressBytes = new Array[Byte](Address.AddressLength)
      Array.copy(e.getKey, 2, addressBytes, 0, Address.AddressLength)
      addresses += Address.fromBytes(addressBytes).explicitGet()
    }

    log.info("Loading stored reserved balances")

    val allReservedBalances = addresses.result().map(a => a -> DBUtils.reservedBalance(db, a)).toMap

    if (allReservedBalances.size != calculatedReservedBalances.size()) {
      log.info(s"Calculated balances: ${calculatedReservedBalances.size()}, stored balances: ${allReservedBalances.size}")
    }

    val corrections = Seq.newBuilder[((Address, Option[AssetId]), Long)]
    var assetsToAdd = Map.empty[Address, Set[Option[AssetId]]]

    for (address <- allReservedBalances.keySet ++ calculatedReservedBalances.keySet().asScala) {
      val calculated = calculatedReservedBalances.getOrDefault(address, Map.empty)
      val stored     = allReservedBalances.getOrElse(address, Map.empty)
      if (calculated != stored) {
        for (assetId <- calculated.keySet ++ stored.keySet) {
          val calculatedBalance = calculated.getOrElse(assetId, 0L)
          val storedBalance     = stored.getOrElse(assetId, 0L)

          if (calculatedBalance != storedBalance) {
            if (!stored.contains(assetId)) assetsToAdd += address -> (assetsToAdd.getOrElse(address, Set.empty) + assetId)
            discrepancyCounter += 1
            corrections += (address, assetId) -> calculatedBalance
          }
        }
      }
    }

    log.info(s"Found $discrepancyCounter discrepancies; writing reserved balances")

    db.readWrite { rw =>
      for ((address, newAssetIds) <- assetsToAdd) {
        val k         = MatcherKeys.openVolumeSeqNr(address)
        val currSeqNr = rw.get(k)

        rw.put(k, currSeqNr + newAssetIds.size)
        for ((assetId, i) <- newAssetIds.zipWithIndex) {
          rw.put(MatcherKeys.openVolumeAsset(address, currSeqNr + 1 + i), assetId)
        }
      }

      for (((address, assetId), value) <- corrections.result()) {
        rw.put(MatcherKeys.openVolume(address, assetId), Some(value))
      }
    }

    val allUpdatedOrderInfo = orderInfoToUpdate.result()
    if (allUpdatedOrderInfo.nonEmpty) {
      log.info(s"Writing ${allUpdatedOrderInfo.size} updated order info values")

      db.readWrite { rw =>
        for ((id, oi) <- allUpdatedOrderInfo) {
          rw.put(MatcherKeys.orderInfo(id), oi)
        }
      }
    }

    log.info("Completed")
  }

  def main(args: Array[String]): Unit = {
    log.info(s"OK, engine start")

    val userConfig = args.headOption.fold(ConfigFactory.empty())(f => ConfigFactory.parseFile(new File(f)))
    val settings   = WavesSettings.fromConfig(loadConfig(userConfig))
    val db         = openDB(settings.matcherSettings.dataDir)

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    if (args(1) == "stats") {
      collectStats(db)
    } else if (args(1) == "ao") {
      val o = DBUtils.ordersByAddress(db, Address.fromString(args(2)).explicitGet(), Set.empty, false, Int.MaxValue)
      println(o.mkString("\n"))
    } else if (args(1) == "cb") {
      recalculateReservedBalance(db)
    } else if (args(1) == "rb") {
      for ((assetId, balance) <- DBUtils.reservedBalance(db, Address.fromString(args(2)).explicitGet())) {
        log.info(s"${AssetPair.assetIdStr(assetId)}: $balance")
      }
    } else if (args(1) == "ddd") {
      log.warn("DELETING LEGACY ENTRIES")
      deleteLegacyEntries(db)
      log.info("Finished deleting legacy entries")
    } else if (args(1) == "compact") {
      log.info("Compacting database")
      db.compactRange(null, null)
      log.info("Compaction complete")
    }

    db.close()
  }

  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  class SK[A](suffix: String, extractor: Array[Byte] => Option[A]) {
    val keyBytes = ("matcher:" + suffix + ":").getBytes(UTF_8)
    def unapply(bytes: Array[Byte]): Option[A] = {
      val (prefix, suffix) = bytes.splitAt(keyBytes.length)
      if (prefix.sameElements(keyBytes)) extractor(suffix) else None
    }
  }

  object SK {
    def apply[A](suffix: String, extractor: Array[Byte] => Option[A]) = new SK(suffix, extractor)

    private def byteStr(b: Array[Byte]) = ByteStr.decodeBase58(new String(b, UTF_8)).toOption
    private def addr(b: Array[Byte])    = Address.fromString(new String(b, UTF_8)).toOption

    val Orders                = SK("orders", byteStr)
    val OrdersInfo            = SK("infos", byteStr)
    val AddressToOrders       = SK("addr-orders", addr)
    val AddressToActiveOrders = SK("a-addr-orders", addr)
    val AddressPortfolio      = SK("portfolios", addr)
    val Transactions          = SK("transactions", byteStr)
    val OrdersToTxIds         = SK("ord-to-tx-ids", byteStr)
  }
}
