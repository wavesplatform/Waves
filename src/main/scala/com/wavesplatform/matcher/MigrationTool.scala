package com.wavesplatform.matcher

import java.io.File
import java.util.HashMap

import com.fasterxml.jackson.databind.ObjectMapper
import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.Shorts
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme, PublicKeyAccount}
import com.wavesplatform.database.DBExt
import com.wavesplatform.db.{AssetIdOrderIdSetCodec, OrderIdsCodec, OrderToTxIdsCodec, PortfolioCodec, openDB}
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.utils.{Base58, ScorexLogging}
import org.iq80.leveldb.DB

import scala.collection.JavaConverters._

object MigrationTool extends ScorexLogging {
  private def loadOrderInfo(db: DB, om: ObjectMapper): Map[ByteStr, OrderInfo] = {
    log.info("Loading order info")
    val result   = Map.newBuilder[ByteStr, OrderInfo]
    val iterator = db.iterator()
    try {
      iterator.seek(SK.OrdersInfo.keyBytes)
      while (iterator.hasNext && iterator.peekNext().getKey.startsWith(SK.OrdersInfo.keyBytes)) {
        val e                 = iterator.next()
        val SK.OrdersInfo(id) = e.getKey
        val t                 = om.readTree(e.getValue)
        val oi                = OrderInfo(t.get("amount").asLong, t.get("filled").asLong, t.get("canceled").asBoolean)
        if (!oi.canceled && oi.amount != oi.filled) {
          result += id -> oi
        }
      }
    } finally iterator.close()
    log.info("Loaded all order infos")
    result.result()
  }

  private def parseOrderJson(om: ObjectMapper, bytes: Array[Byte]): Order = {
    val t = om.readTree(bytes)
    val p = t.get("assetPair")
    Order(
      PublicKeyAccount.fromBase58String(t.get("senderPublicKey").asText()).explicitGet(),
      PublicKeyAccount.fromBase58String(t.get("matcherPublicKey").asText()).explicitGet(),
      AssetPair
        .createAssetPair(
          p.get("amountAsset").asText("WAVES"),
          p.get("priceAsset").asText("WAVES")
        )
        .get,
      OrderType(t.get("orderType").asText()),
      t.get("price").asLong(),
      t.get("amount").asLong(),
      t.get("timestamp").asLong(),
      t.get("expiration").asLong(),
      t.get("matcherFee").asLong(),
      Base58.decode(t.get("signature").asText()).get
    )
  }

  private def performMigration(db: DB): Unit = {
    val addressOrders     = Seq.newBuilder[(Address, Seq[ByteStr])]
    val orderTransactions = Seq.newBuilder[(ByteStr, Set[String])]
    val orders            = Map.newBuilder[ByteStr, Order]
    val transactions      = Seq.newBuilder[(ByteStr, Array[Byte])]
    val portfolios        = Seq.newBuilder[(Address, Map[String, Long])]

    val om = new ObjectMapper()

    val orderInfo = loadOrderInfo(db, om)

    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      while (iterator.hasNext) {
        val entry = iterator.next()
        entry.getKey match {
          case SK.OrdersInfo(_) => // order info has already been processed
          case SK.AddressToOrders(address) =>
            addressOrders += address -> OrderIdsCodec
              .decode(entry.getValue)
              .explicitGet()
              .value
              .map(ByteStr.decodeBase58(_).get)
              .filter(orderInfo.keySet)
              .toSeq
          case SK.OrdersToTxIds(orderId) =>
            if (orderInfo.contains(orderId)) {
              orderTransactions += orderId -> OrderToTxIdsCodec.decode(entry.getValue).explicitGet().value
            }
          case SK.Orders(orderId) =>
            if (orderInfo.contains(orderId)) {
              orders += orderId -> parseOrderJson(om, entry.getValue)
            }
          case SK.Transactions(txId) =>
            transactions += txId -> entry.getValue
          case SK.AddressPortfolio(address) =>
            portfolios += address -> PortfolioCodec.decode(entry.getValue).explicitGet().value
          case SK.AddressToActiveOrders(_) =>
          case _                           =>
        }
      }
    } finally iterator.close()

    val allOrders            = orders.result()
    val allTransactions      = transactions.result()
    val allAddressOrders     = addressOrders.result()
    val allOpenVolume        = portfolios.result()
    val allOrderTransactions = orderTransactions.result()
    log.info(s"""Done reading data:
                |
        |order infos:        ${orderInfo.size}
                |address orders:     ${allAddressOrders.length}
                |order transactions: ${allOrderTransactions.length}
                |orders:             ${allOrders.size}
                |transactions:       ${allTransactions.length}
                |portfolios:         ${allOpenVolume.length}
      """.stripMargin)

    db.readWrite { rw =>
      log.info("Saving orders")
      for ((id, order) <- allOrders) {
        rw.put(MatcherKeys.order(id), Some(order))
      }
      log.info("Saving order info")
      for ((id, info) <- orderInfo) {
        rw.put(MatcherKeys.orderInfo(id), info)
      }
      log.info("Saving orders for address")
      for ((address, orderIds) <- allAddressOrders) {
        val activeOrderIds = orderIds.filter(orderInfo.keySet)
        rw.put(MatcherKeys.addressOrdersSeqNr(address), activeOrderIds.size)
        for ((id, offset) <- activeOrderIds.zipWithIndex) {
          rw.put(MatcherKeys.addressOrders(address, offset + 1), OrderAssets(id, allOrders(id).getSpendAssetId))
        }
      }
      log.info("Saving open volume")
      for ((address, ov) <- allOpenVolume) {
        val assetCount = ov.size
        rw.put(MatcherKeys.openVolumeSeqNr(address), assetCount)
        for (((assetIdStr, v), offset) <- ov.zipWithIndex) {
          val assetId = AssetPair.extractAssetId(assetIdStr).get
          rw.put(MatcherKeys.openVolume(address, assetId), Some(v))
          rw.put(MatcherKeys.openVolumeAsset(address, offset + 1), assetId)
        }
      }
      log.info("Saving order transactions")
      for ((orderId, txIds) <- allOrderTransactions) {
        val txCount = txIds.size
        rw.put(MatcherKeys.orderTxIdsSeqNr(orderId), txCount)
        for ((id, offset) <- txIds.flatMap(ByteStr.decodeBase58(_).toOption).zipWithIndex) {
          rw.put(MatcherKeys.orderTxId(orderId, offset + 1), id)
        }
      }
      log.info("Saving transactions")
      for ((id, txBytes) <- allTransactions) {
        rw.put(MatcherKeys.exchangeTransaction(id).keyBytes, txBytes)
      }

      log.info("Writing changes")
    }

    log.info("Migration complete")
  }

  private def collectStats(db: DB): Unit = {
    log.info("Collecting stats")
    val iterator = db.iterator()
    iterator.seekToFirst()

    val result = new HashMap[Short, Stats]

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

  private def migrateActiveOrders(db: DB): Unit = {
    log.info("Migrating active orders")
    val prefix        = "matcher:a-addr-orders:".getBytes(UTF_8)
    val iterator      = db.iterator()
    val addressOrders = Seq.newBuilder[(Address, Set[OrderAssets])]
    try {
      iterator.seek(prefix)
      while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)) {
        val e                              = iterator.next()
        val SK.AddressToActiveOrders(addr) = e.getKey
        val orderIds = AssetIdOrderIdSetCodec.decode(e.getValue).explicitGet().value.map {
          case (assetId, orderIdStr) => OrderAssets(ByteStr.decodeBase58(orderIdStr).get, assetId)
        }

        addressOrders += addr -> orderIds
      }
    } finally iterator.close()

    val r = addressOrders.result()

    log.info(s"Collected active orders for ${r.size} addresses")

    db.readWrite { rw =>
      for ((addr, migratedOrders) <- r) {
        val currentOrderCount = rw.get(MatcherKeys.addressOrdersSeqNr(addr))
        val currentOrderAssets = (1 to currentOrderCount).map { i =>
          rw.get(MatcherKeys.addressOrders(addr, i))
        }.toSet

        val ordersToAdd = migratedOrders.diff(currentOrderAssets)
        if (ordersToAdd.nonEmpty) {
          rw.put(MatcherKeys.addressOrdersSeqNr(addr), currentOrderCount + ordersToAdd.size)
          for ((oa, offset) <- ordersToAdd.zipWithIndex) {
            rw.put(MatcherKeys.addressOrders(addr, currentOrderCount + offset + 1), oa)
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    log.info(s"OK, engine start")

    val userConfig = args.headOption.fold(ConfigFactory.empty())(f => ConfigFactory.parseFile(new File(f)))
    val settings   = WavesSettings.fromConfig(loadConfig(userConfig))
    val db         = openDB(settings.matcherSettings.dataDir)

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    if (args.length < 2) {
      log.info("Performing migration")
      performMigration(db)
    } else if (args(1) == "stats") {
      collectStats(db)
    } else if (args(1) == "active-orders") {
      migrateActiveOrders(db)
    } else if (args(1) == "ao") {
      val o = DBUtils.ordersByAddress(db, Address.fromString(args(2)).explicitGet(), Set.empty, false)
      println(o.mkString("\n"))
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
