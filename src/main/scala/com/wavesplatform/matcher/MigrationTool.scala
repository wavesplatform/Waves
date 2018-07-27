package com.wavesplatform.matcher

import java.io.File

import com.fasterxml.jackson.databind.ObjectMapper
import com.google.common.base.Charsets.UTF_8
import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.DBExt
import com.wavesplatform.db.{OrderIdsCodec, OrderToTxIdsCodec, PortfolioCodec, openDB}
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.utils.Base58
import org.iq80.leveldb.DB
import scorex.account.{Address, AddressScheme, PublicKeyAccount}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.utils.ScorexLogging

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
        result += id -> oi
      }
    } finally iterator.close()
    log.info("Loaded all order infos")
    result.result()
  }

  private def parseOrderJson(om: ObjectMapper, bytes: Array[Byte]): Option[Order] = {
    val t         = om.readTree(bytes)
    val timestamp = t.get("timestamp").asLong()
    if (System.currentTimeMillis() - timestamp > 30 * 24 * 60 * 60 * 1000L) None
    else {
      val p = t.get("assetPair")

      Some(
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
          timestamp,
          t.get("expiration").asLong(),
          t.get("matcherFee").asLong(),
          Base58.decode(t.get("signature").asText()).get
        ))
    }
  }

  def main(args: Array[String]): Unit = {
    val userConfig = args.headOption.fold(ConfigFactory.empty())(f => ConfigFactory.parseFile(new File(f)))
    val settings   = WavesSettings.fromConfig(loadConfig(userConfig))
    val db         = openDB(settings.matcherSettings.dataDir)

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val orderInfos        = Seq.newBuilder[(ByteStr, OrderInfo)]
    val addressOrders     = Seq.newBuilder[(Address, Seq[String])]
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
          case SK.OrdersInfo(id) =>
            orderInfos += id -> OrderInfo.empty
          case SK.AddressToOrders(address) =>
            addressOrders += address -> OrderIdsCodec.decode(entry.getValue).explicitGet().value
          case SK.OrdersToTxIds(orderId) =>
            orderTransactions += orderId -> OrderToTxIdsCodec.decode(entry.getValue).explicitGet().value
          case SK.Orders(orderId) =>
            for (o <- parseOrderJson(om, entry.getValue)) {
              orders += orderId -> o
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

    val allOrders        = orders.result()
    val allTransactions  = transactions.result()
    val allAddressOrders = addressOrders.result()
    val allOpenVolume    = portfolios.result()
    log.info(s"""Done reading data:
        |
        |order infos:        ${orderInfos.result().length}
        |address orders:     ${allAddressOrders.length}
        |order transactions: ${orderTransactions.result().length}
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
        val activeOrderIds = orderIds.flatMap(ByteStr.decodeBase58(_).toOption).filter(orderInfo.keySet)
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
      log.info("Writing changes")
    }

    db.close()

    log.info("Migration complete")
  }

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
