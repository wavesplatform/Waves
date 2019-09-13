package com.wavesplatform

import java.io.File
import java.nio.ByteBuffer
import java.util

import com.google.common.primitives.Shorts
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter, openDB}
import com.wavesplatform.state.{Height, TxNum}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Transaction, TransactionParsers}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

//noinspection ScalaStyle
object Explorer extends ScorexLogging {
  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  private val keys = Array(
    "version",
    "height",
    "score",
    "block-at-height", // not used now
    "height-of",
    "waves-balance-history",
    "waves-balance",
    "assets-for-address",
    "asset-balance-history",
    "asset-balance",
    "asset-info-history",
    "asset-info",
    "lease-balance-history",
    "lease-balance",
    "lease-status-history",
    "lease-status",
    "filled-volume-and-fee-history",
    "filled-volume-and-fee",
    "transaction-info", // not used now
    "address-transaction-history",
    "address-transaction-ids-at-height",
    "changed-addresses",
    "transaction-ids-at-height", // not used now
    "address-id-of-alias",
    "last-address-id",
    "address-to-id",
    "id-of-address",
    "address-script-history",
    "address-script",
    "approved-features",
    "activated-features",
    "data-key-chunk-count",
    "data-key-chunk",
    "data-history",
    "data",
    "sponsorship-history",
    "sponsorship",
    "addresses-for-waves-seq-nr",
    "addresses-for-waves",
    "addresses-for-asset-seq-nr",
    "addresses-for-asset",
    "address-transaction-ids-seq-nr", // not used now
    "address-transaction-ids",        // not used now
    "alias-is-disabled",
    "carry-fee-history",
    "carry-fee",
    "asset-script-history",
    "asset-script",
    "safe-rollback-height",
    "changed-data-keys",
    "block-header-at-height",
    "nth-transaction-info-at-height",
    "address-transaction-seq-nr",
    "address-transaction-height-type-and-nums",
    "transaction-height-and-nums-by-id",
    "block-transactions-fee",
    "invoke-script-result",
    "block-reward"
  )

  def main(argsRaw: Array[String]): Unit = {
    if (argsRaw.isEmpty) {
      System.err.println("Usage: waves explore <command> [args] [--config|-c <cfg file>]")
      return
    }

    @tailrec
    def parseArgs(buffer: Seq[String], args: Seq[String] = Nil, flags: Map[String, String] = Map.empty): (Seq[String], Map[String, String]) =
      buffer match {
        case flag +: value +: rest if flag.startsWith("-") =>
          parseArgs(rest, args, flags + (flag -> value))

        case arg +: rest =>
          parseArgs(rest, args :+ arg, flags)

        case Nil =>
          (args, flags)
      }

    val (args, flags)    = parseArgs(argsRaw)
    val configFileOption = flags.collectFirst { case ("-c" | "--config", config) if config.nonEmpty => new File(config) }

    val settings = Application.loadApplicationConfig(configFileOption)

    log.info(s"Data directory: ${settings.dbSettings.directory}")

    val portfolioChanges = Observer.empty(UncaughtExceptionReporter.default)
    val db               = openDB(settings.dbSettings.directory)
    val reader           = new LevelDBWriter(db, portfolioChanges, settings.blockchainSettings.functionalitySettings, settings.dbSettings)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    try {
      @inline
      def argument(i: Int, msg: => String) = args.applyOrElse(i, (_: Int) => throw new IllegalArgumentException(s"Argument #${i + 1} missing: $msg"))
      val flag                             = argument(0, "command").toUpperCase

      flag match {
        case "B" =>
          val maybeBlockId = Base58.tryDecodeWithLimit(argument(1, "block id")).toOption.map(ByteStr.apply)
          if (maybeBlockId.isDefined) {
            val kBlockHeight     = Keys.heightOf(maybeBlockId.get)
            val blockHeightBytes = db.get(kBlockHeight.keyBytes)
            val maybeBlockHeight = kBlockHeight.parse(blockHeightBytes)
            maybeBlockHeight.foreach { h =>
              val kBlock     = Keys.blockHeaderBytesAt(Height(h))
              val blockBytes = db.get(kBlock.keyBytes)
              log.info(s"BlockId=${maybeBlockId.get} at h=$h: ${Base64.encode(blockBytes)}")
            }
          } else log.error("No block ID was provided")

        case "O" =>
          val orderId = Base58.tryDecodeWithLimit(argument(1, "order id")).toOption.map(ByteStr.apply)
          if (orderId.isDefined) {
            val kVolumeAndFee = Keys.filledVolumeAndFee(orderId.get)(blockchainHeight)
            val bytes1        = db.get(kVolumeAndFee.keyBytes)
            val v             = kVolumeAndFee.parse(bytes1)
            log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: Volume = ${v.volume}, Fee = ${v.fee}")

            val kVolumeAndFeeHistory = Keys.filledVolumeAndFeeHistory(orderId.get)
            val bytes2               = db.get(kVolumeAndFeeHistory.keyBytes)
            val value2               = kVolumeAndFeeHistory.parse(bytes2)
            val value2Str            = value2.mkString("[", ", ", "]")
            log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: History = $value2Str")
            value2.foreach { h =>
              val k = Keys.filledVolumeAndFee(orderId.get)(h)
              val v = k.parse(db.get(k.keyBytes))
              log.info(s"\t h = $h: Volume = ${v.volume}, Fee = ${v.fee}")
            }
          } else log.error("No order ID was provided")

        case "A" =>
          val address   = Address.fromString(argument(1, "address")).explicitGet()
          val aid       = Keys.addressId(address)
          val addressId = aid.parse(db.get(aid.keyBytes)).get
          log.info(s"Address id = $addressId")

          val kwbh = Keys.wavesBalanceHistory(addressId)
          val wbh  = kwbh.parse(db.get(kwbh.keyBytes))

          val balances = wbh.map { h =>
            val k = Keys.wavesBalance(addressId)(h)
            h -> k.parse(db.get(k.keyBytes))
          }
          balances.foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))

        case "AC" =>
          val lastAddressId = Keys.lastAddressId.parse(db.get(Keys.lastAddressId.keyBytes))
          log.info(s"Last address id: $lastAddressId")

        case "AD" =>
          val result        = new util.HashMap[Address, java.lang.Integer]()
          val lastAddressId = Keys.lastAddressId.parse(db.get(Keys.lastAddressId.keyBytes))
          for (id <- BigInt(1) to lastAddressId.getOrElse(BigInt(0))) {
            val k       = Keys.idToAddress(id)
            val address = k.parse(db.get(k.keyBytes))
            result.compute(
              address,
              (_, prev) =>
                prev match {
                  case null    => 1
                  case notNull => 1 + notNull
                }
            )
          }

          for ((k, v) <- result.asScala if v > 1) {
            log.info(s"$k,$v")
          }

        case "AA" =>
          val address   = Address.fromString(argument(1, "address")).explicitGet()
          val asset     = IssuedAsset(ByteStr.decodeBase58(argument(2, "asset")).get)
          val ai        = Keys.addressId(address)
          val addressId = ai.parse(db.get(ai.keyBytes)).get
          log.info(s"Address ID = $addressId")

          val kabh = Keys.assetBalanceHistory(addressId, asset)
          val abh  = kabh.parse(db.get(kabh.keyBytes))

          val balances = abh.map { h =>
            val k = Keys.assetBalance(addressId, asset)(h)
            h -> k.parse(db.get(k.keyBytes))
          }
          balances.foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))

        case "S" =>
          log.info("Collecting DB stats")
          val iterator = db.iterator()
          val result   = new util.HashMap[Short, Stats]
          iterator.seekToFirst()
          while (iterator.hasNext) {
            val entry     = iterator.next()
            val keyPrefix = ByteBuffer.wrap(entry.getKey).getShort
            result.compute(
              keyPrefix,
              (_, maybePrev) =>
                maybePrev match {
                  case null => Stats(1, entry.getKey.length, entry.getValue.length)
                  case prev => Stats(prev.entryCount + 1, prev.totalKeySize + entry.getKey.length, prev.totalValueSize + entry.getValue.length)
                }
            )
          }
          iterator.close()

          log.info("key-space,entry-count,total-key-size,total-value-size")
          for ((prefix, stats) <- result.asScala) {
            log.info(s"${keys(prefix)},${stats.entryCount},${stats.totalKeySize},${stats.totalValueSize}")
          }

        case "TXBH" =>
          val txs = new ListBuffer[(TxNum, Transaction)]

          val h = Height(argument(1, "height").toInt)

          val prefix = ByteBuffer
            .allocate(6)
            .putShort(Keys.TransactionInfoPrefix)
            .putInt(h)
            .array()

          val iterator = db.iterator

          try {
            iterator.seek(prefix)
            while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)) {
              val entry = iterator.next()

              val k = entry.getKey
              println(k.toList.map(_.toInt & 0xff))
              val v = entry.getValue

              for {
                idx <- Try(Shorts.fromByteArray(k.slice(6, 8)))
                tx  <- TransactionParsers.parseBytes(v)
              } txs.append((TxNum(idx), tx))
            }
          } finally iterator.close()

          println(txs.length)
          txs.foreach(println)

        case "AP" =>
          val address   = Address.fromString(argument(1, "address")).explicitGet()
          val portfolio = reader.portfolio(address)
          log.info(s"$address : ${portfolio.balance} WAVES, ${portfolio.lease}, ${portfolio.assets.size} assets")
          portfolio.assets.toSeq.sortBy(_._1.toString) foreach {
            case (assetId, balance) => log.info(s"$assetId : $balance")
          }

        case "APS" =>
          val addrs = mutable.Set[Address]()

          println(s"\nWAVES balances\n")
          for {
            seqNr     <- (1 to db.get(Keys.addressesForWavesSeqNr)).par
            addressId <- db.get(Keys.addressesForWaves(seqNr)).par
            history = db.get(Keys.wavesBalanceHistory(addressId))
            actualHeight <- history.headOption
            balance = db.get(Keys.wavesBalance(addressId)(actualHeight))
            if balance > 0
          } yield {
            val addr = db.get(Keys.idToAddress(addressId))
            println(s"$addr : $balance")
            addrs += addr
          }

          val assets = mutable.ListBuffer[ByteStr]()
          db.iterateOver(10: Short) { e => // iterate over Keys.assetInfoHistory
            assets += ByteStr(e.getKey.drop(2))
          }

          println(s"\nAssets balances (${assets.size} assets)")
          for {
            assetId <- assets.sorted
            asset = IssuedAsset(assetId)
            seqNr <- {
              println(s"\n$assetId:")
              1 to db.get(Keys.addressesForAssetSeqNr(asset))
            }
            addressId    <- db.get(Keys.addressesForAsset(asset, seqNr))
            actualHeight <- db.get(Keys.assetBalanceHistory(addressId, asset)).headOption
            balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
            if balance > 0
          } yield {
            val addr = db.get(Keys.idToAddress(addressId))
            println(s"$addr : $balance")
            addrs += addr
          }

          println(s"\nAddress balances (${addrs.size} addresses)")
          addrs.toSeq.sortBy(_.toString).foreach { addr =>
            val p = reader.portfolio(addr)
            println(s"\n$addr : ${p.balance} ${p.lease}:")
            p.assets.toSeq.sortBy(_._1.id).foreach {
              case (IssuedAsset(assetId), bal) =>
                if (bal > 0)
                  println(s"$assetId : $bal")
            }
          }
      }
    } finally db.close()
  }
}
