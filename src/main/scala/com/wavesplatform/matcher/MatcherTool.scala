package com.wavesplatform.matcher

import java.io.File
import java.util.{HashMap => JHashMap}

import akka.actor.ActorSystem
import akka.persistence.query.PersistenceQuery
import akka.persistence.query.journal.leveldb.scaladsl.LeveldbReadJournal
import akka.persistence.serialization.Snapshot
import akka.serialization.SerializationExtension
import akka.stream.ActorMaterializer
import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.Shorts
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database._
import com.wavesplatform.db.openDB
import com.wavesplatform.matcher.db.AssetPairsDB
import com.wavesplatform.matcher.market.{MatcherActor, OrderBookActor}
import com.wavesplatform.matcher.model.{LimitOrder, OrderBook}
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MatcherTool extends ScorexLogging {
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

    db.iterateOver("matcher:".getBytes(UTF_8))(e => keysToDelete += e.getKey)

    db.readWrite(rw => keysToDelete.result().foreach(rw.delete(_, "matcher-legacy-entries")))
  }

  def main(args: Array[String]): Unit = {
    log.info(s"OK, engine start")

    val userConfig   = args.headOption.fold(ConfigFactory.empty())(f => ConfigFactory.parseFile(new File(f)))
    val actualConfig = loadConfig(userConfig)
    val settings     = WavesSettings.fromConfig(actualConfig)
    val db           = openDB(settings.matcherSettings.dataDir)

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val start = System.currentTimeMillis()
    args(1) match {
      case "stats" => collectStats(db)
      case "ob" =>
        val pair   = AssetPair.createAssetPair(args(2), args(3)).get
        val system = ActorSystem("matcher-tool", actualConfig)
        val se     = SerializationExtension(system)
        try {
          val snapshotDB    = openDB(settings.matcherSettings.snapshotsDataDir)
          val persistenceId = OrderBookActor.name(pair)

          val historyKey = MatcherSnapshotStore.kSMHistory(persistenceId)
          val history    = historyKey.parse(snapshotDB.get(historyKey.keyBytes))
          println(s"Snapshots history for $pair: $history")

          history.headOption.foreach { lastSnapshotNr =>
            val lastSnapshotKey     = MatcherSnapshotStore.kSnapshot(persistenceId, lastSnapshotNr)
            val lastSnapshotRawData = lastSnapshotKey.parse(snapshotDB.get(lastSnapshotKey))
            val lastSnapshot        = se.deserialize(lastSnapshotRawData, classOf[Snapshot]).get.data.asInstanceOf[OrderBookActor.Snapshot].orderBook

            def formatOrders(v: Iterable[LimitOrder]) =
              (for {
                lo <- v
              } yield s"${lo.order.id()} -> $lo").mkString("\n")

            println(s"Last snapshot: $lastSnapshot")
            println(s"Orders:\n${formatOrders(OrderBook(lastSnapshot).allOrders)}")
          }
        } finally {
          Await.ready(system.terminate(), Duration.Inf)
        }
      case "ob-compact" =>
        val groupSize = args(2).toInt

        log.info("Removing stale snapshots from order books")
        log.info("Warning: matcher's snapshot in new format is required")
        val system = ActorSystem("matcher-tool", actualConfig)
        try {
          val apdb  = AssetPairsDB(db)
          val pairs = apdb.all().toVector
          log.info(s"Found ${pairs.size} asset pairs")

          val snapshotDB = openDB(settings.matcherSettings.snapshotsDataDir)
          pairs.grouped(groupSize).foreach { pairs =>
            snapshotDB.readWrite { rw =>
              pairs.foreach { pair =>
                val persistenceId = OrderBookActor.name(pair)
                val history       = rw.get(MatcherSnapshotStore.kSMHistory(persistenceId))
                if (history.lengthCompare(1) > 0) {
                  log.info(s"Snapshots history for $pair has ${history.size} records")
                  val (toKeep, toDelete) = history.map(seqNr => seqNr -> rw.get(MatcherSnapshotStore.kSM(persistenceId, seqNr))).splitAt(1)
                  rw.put(MatcherSnapshotStore.kSMHistory(persistenceId), toKeep.map(_._1).sorted.reverse)
                  for ((seqNr, _) <- toDelete) {
                    rw.delete(MatcherSnapshotStore.kSM(persistenceId, seqNr))
                    rw.delete(MatcherSnapshotStore.kSnapshot(persistenceId, seqNr))
                  }
                }
              }
            }
          }
        } finally {
          Await.ready(system.terminate(), Duration.Inf)
        }
      case "oi" =>
        val id = ByteStr.decodeBase58(args(2)).get
        println(s"Loading order '$id'")

        val orderKey = MatcherKeys.order(id)
        orderKey.parse(db.get(orderKey.keyBytes)).foreach { o =>
          println(s"Order (id=${o.id()}): $o")
        }

        val orderInfoKey = MatcherKeys.orderInfo(id)
        orderInfoKey.parse(db.get(orderInfoKey.keyBytes)).foreach { oi =>
          println(s"Order info: $oi")
        }
      case "ddd" =>
        log.warn("DELETING LEGACY ENTRIES")
        deleteLegacyEntries(db)
      case "compact" =>
        log.info("Compacting database")
        db.compactRange(null, null)
      case "dex-compact" =>
        log.info("Compacting DEX database")
        val snapshotDB = openDB(settings.matcherSettings.snapshotsDataDir)
        snapshotDB.compactRange(null, null)
      case "ma" =>
        val system = ActorSystem("matcher-tool", actualConfig)
        try {
          val snapshotDB    = openDB(settings.matcherSettings.snapshotsDataDir)
          val persistenceId = MatcherActor.name
          val se            = SerializationExtension(system)

          val historyKey = MatcherSnapshotStore.kSMHistory(persistenceId)
          val history    = historyKey.parse(snapshotDB.get(historyKey.keyBytes))
          if (history.isEmpty) log.warn("History is empty")
          else {
            log.info(s"Snapshots history for ${MatcherActor.name}: $history")
            val xs = history.map { seqNr =>
              val smKey = MatcherSnapshotStore.kSM(persistenceId, seqNr)
              val smRaw = snapshotDB.get(smKey.keyBytes)
              val sm    = smKey.parse(smRaw)

              val snapshotKey = MatcherSnapshotStore.kSnapshot(persistenceId, seqNr)
              val snapshotRaw = snapshotDB.get(snapshotKey.keyBytes)

              (seqNr, sm, snapshotRaw)
            }
            log.info(s"Records:\n${xs.map { case (seqNr, sm, _) => s"$seqNr: SM(seqNr=${sm.seqNr}, ts=${sm.ts})" }.mkString("\n")}")

            val (_, _, lastSnapshotRaw) = xs.last
            val snapshot                = se.deserialize(lastSnapshotRaw, classOf[Snapshot]).get.data.asInstanceOf[MatcherActor.Snapshot]
            log.info(s"The last snapshot:\n${snapshot.tradedPairsSet.map(_.key).toVector.sorted.mkString("\n")}")
          }
        } finally {
          Await.ready(system.terminate(), Duration.Inf)
        }
      case "maj" =>
        val from   = args(2).toLong
        val to     = if (args.length < 4) Long.MaxValue else args(3).toLong
        val system = ActorSystem("matcher-tool", actualConfig)
        val mat    = ActorMaterializer()(system)
        try {
          val readJournal = PersistenceQuery(system).readJournalFor[LeveldbReadJournal](LeveldbReadJournal.Identifier)
          val query       = readJournal.currentEventsByPersistenceId(MatcherActor.name, from, to)
          val process = query.runForeach { rawEvt =>
            val evt = rawEvt.event.asInstanceOf[MatcherActor.OrderBookCreated]
            log.info(s"[offset=${rawEvt.offset}, seqNr=${rawEvt.sequenceNr}] $evt")
          }(mat)

          Await.ready(process, Duration.Inf)
        } finally {
          mat.shutdown()
          Await.ready(system.terminate(), Duration.Inf)
        }
      case "ma-migrate" =>
        val system = ActorSystem("matcher-tool", actualConfig)
        val mat    = ActorMaterializer()(system)
        try {
          val allPairs      = Set.newBuilder[AssetPair]
          val snapshotDB    = openDB(settings.matcherSettings.snapshotsDataDir)
          val persistenceId = MatcherActor.name
          val se            = SerializationExtension(system)

          val historyKey = MatcherSnapshotStore.kSMHistory(persistenceId)
          val history    = historyKey.parse(snapshotDB.get(historyKey.keyBytes))
          if (history.isEmpty) log.warn("Snapshots history is empty")
          else {
            val xs = history.map { seqNr =>
              val smKey = MatcherSnapshotStore.kSM(persistenceId, seqNr)
              val smRaw = snapshotDB.get(smKey.keyBytes)
              val sm    = smKey.parse(smRaw)

              val snapshotKey = MatcherSnapshotStore.kSnapshot(persistenceId, seqNr)
              val snapshotRaw = snapshotDB.get(snapshotKey.keyBytes)

              (seqNr, sm, snapshotRaw)
            }
            log.info(s"Records:\n${xs.map { case (seqNr, sm, _) => s"$seqNr: SM(seqNr=${sm.seqNr}, ts=${sm.ts})" }.mkString("\n")}")

            val (_, _, lastSnapshotRaw) = xs.last
            val snapshot                = se.deserialize(lastSnapshotRaw, classOf[Snapshot]).get.data.asInstanceOf[MatcherActor.Snapshot]
            log.info(s"The last snapshot:\n${snapshot.tradedPairsSet.map(_.key).toVector.sorted.mkString("\n")}")
            allPairs ++= snapshot.tradedPairsSet
          }

          val readJournal = PersistenceQuery(system).readJournalFor[LeveldbReadJournal](LeveldbReadJournal.Identifier)
          val query       = readJournal.currentEventsByPersistenceId(MatcherActor.name, 0)
          val process = query.runForeach { rawEvt =>
            val evt = rawEvt.event.asInstanceOf[MatcherActor.OrderBookCreated]
            log.info(s"[offset=${rawEvt.offset}, seqNr=${rawEvt.sequenceNr}] $evt")
            allPairs += evt.assetPair
          }(mat)

          Await.ready(process, Duration.Inf)
          log.info(s"Asset pairs collected")

          val apdb = AssetPairsDB(db)
          allPairs.result().foreach(apdb.add)
          log.info(s"Asset pairs migrated")
        } finally {
          mat.shutdown()
          Await.ready(system.terminate(), Duration.Inf)
        }
      case "ma-inspect" =>
        val apdb            = AssetPairsDB(db)
        val knownAssetPairs = apdb.all()
        if (knownAssetPairs.isEmpty) log.info("There are no known asset pairs")
        else log.info(s"Known asset pairs: ${knownAssetPairs.mkString("\n")}")
      case _ =>
    }

    log.info(s"Completed in ${(System.currentTimeMillis() - start) / 1000}s")
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
