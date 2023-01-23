package com.wavesplatform

import java.io.File
import java.nio.ByteBuffer
import java.util
import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressPortfolio
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.database.*
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.{DiffsCommon, SetScriptTransactionDiff}
import com.wavesplatform.state.{Blockchain, Diff, Height, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

import scala.annotation.tailrec
import scala.collection.immutable.VectorMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

//noinspection ScalaStyle
object Explorer extends ScorexLogging {
  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  def portfolio(db: DB, blockchain: Blockchain, address: Address): Portfolio =
    Portfolio(
      blockchain.balance(address),
      blockchain.leaseBalance(address),
      db.withResource(r => AddressPortfolio.assetBalanceIterator(r, address, Diff.empty, _ => true).to(VectorMap))
    )

  def main(argsRaw: Array[String]): Unit = {
    if (argsRaw.isEmpty || Seq("-h", "--help").exists(argsRaw.contains)) {
      System.err.println("Usage: waves explore <command> [args] [--config|-c <cfg file>]")
      return
    }

    @tailrec
    def parseArgs(buffer: Seq[String], args: Seq[String] = Nil, flags: Map[String, String] = Map.empty): (Seq[String], Map[String, String]) =
      (buffer: @unchecked) match {
        case flag +: value +: rest if flag.startsWith("-") =>
          parseArgs(rest, args, flags + (flag -> value))

        case arg +: rest =>
          parseArgs(rest, args :+ arg, flags)

        case Nil =>
          (args, flags)
      }

    val (args, flags)    = parseArgs(argsRaw.toIndexedSeq)
    val configFileOption = flags.collectFirst { case ("-c" | "--config", config) if config.nonEmpty => new File(config) }

    val settings = Application.loadApplicationConfig(configFileOption)

    log.info(s"Data directory: ${settings.dbSettings.directory}")

    val db     = openDB(settings.dbSettings.directory)
    val reader = LevelDBWriter.readOnly(db, settings)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    try {
      @inline
      def argument(i: Int, msg: => String) = args.applyOrElse(i, (_: Int) => throw new IllegalArgumentException(s"Argument #${i + 1} missing: $msg"))
      val flag                             = argument(0, "command").toUpperCase

      flag match {
        case "WB" =>
          val balances = mutable.Map[BigInt, Long]()
          db.iterateOver(KeyTags.WavesBalance) { e =>
            val addressId = BigInt(e.getKey.drop(6))
            val balance   = Longs.fromByteArray(e.getValue)
            balances += (addressId -> balance)
          }

          var actualTotalReward = 0L
          db.iterateOver(KeyTags.BlockReward) { e =>
            actualTotalReward += Longs.fromByteArray(e.getValue)
          }

          val actualTotalBalance   = balances.values.sum + reader.carryFee
          val expectedTotalBalance = Constants.UnitsInWave * Constants.TotalWaves + actualTotalReward
          val byKeyTotalBalance    = reader.wavesAmount(blockchainHeight)

          if (actualTotalBalance != expectedTotalBalance || expectedTotalBalance != byKeyTotalBalance)
            log.error(
              s"Something wrong, actual total waves balance: $actualTotalBalance," +
                s" expected total waves balance: $expectedTotalBalance, total waves balance by key: $byKeyTotalBalance"
            )
          else
            log.info(s"Correct total waves balance: $actualTotalBalance WAVELETS")

        case "DA" =>
          val addressIds = mutable.Seq[(BigInt, Address)]()
          db.iterateOver(KeyTags.AddressId) { e =>
            val address   = Address.fromBytes(e.getKey.drop(2), settings.blockchainSettings.addressSchemeCharacter.toByte)
            val addressId = BigInt(e.getValue)
            addressIds :+ (addressId -> address)
          }
          val addressIdToAddresses = addressIds.groupBy(_._1).view.mapValues(_.map(_._2))

          addressIdToAddresses.find(_._2.size > 1) match {
            case Some((addressId, addresses)) => log.error(s"Something wrong, addressId is duplicated: $addressId for (${addresses.mkString(", ")})")
            case None                         => log.info("Correct address ids")
          }

        case "B" =>
          val maybeBlockId = Base58.tryDecodeWithLimit(argument(1, "block id")).toOption.map(ByteStr.apply)
          if (maybeBlockId.isDefined) {
            val kBlockHeight     = Keys.heightOf(maybeBlockId.get)
            val blockHeightBytes = db.get(kBlockHeight.keyBytes)
            val maybeBlockHeight = kBlockHeight.parse(blockHeightBytes)
            maybeBlockHeight.foreach { h =>
              val kBlock     = Keys.blockInfoBytesAt(Height(h))
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
          val result = new util.HashMap[Address, java.lang.Integer]()

          db.iterateOver(KeyTags.IdToAddress) { e =>
            result.compute(
              Address.fromBytes(e.getValue).explicitGet(),
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
            log.info(s"${KeyTags(prefix)},${stats.entryCount},${stats.totalKeySize},${stats.totalValueSize}")
          }

        case "TXBH" =>
          val h   = Height(argument(1, "height").toInt)
          val txs = db.readOnly(loadTransactions(h, _))

          println(txs.length)
          txs.foreach { case (_, tx) => println(tx) }

        case "AP" =>
          val address = Address.fromString(argument(1, "address")).explicitGet()
          val pf      = portfolio(db, reader, address)
          log.info(s"$address : ${pf.balance} WAVES, ${pf.lease}, ${pf.assets.size} assets")
          pf.assets.toSeq.sortBy(_._1.toString) foreach {
            case (assetId, balance) => log.info(s"$assetId : $balance")
          }

        case "HS" =>
          val height       = argument(1, "height").toInt
          val hitSourceKey = Keys.hitSource(height)
          val hitSource    = db.get(hitSourceKey.keyBytes)
          log.info(s"HitSource at height=$height: ${Base64.encode(hitSource)}")

        case "OC" =>
          log.info("Counting orders")
          var counter = 0L
          db.iterateOver(KeyTags.FilledVolumeAndFeeHistory) { _ =>
            counter += 1
          }
          log.info(s"Found $counter orders")

        case "CAT" =>
          log.info(s"Counting address transactions")
          val addressCount = db.get(Keys.lastAddressId).get.toInt
          log.info(s"Processing $addressCount addresses")
          val txCounts = new Array[Int](addressCount + 1)
          db.iterateOver(KeyTags.AddressTransactionHeightTypeAndNums) { e =>
            txCounts(Longs.fromByteArray(e.getKey.slice(2, 10)).toInt) += readTransactionHNSeqAndType(e.getValue)._2.size
          }
          log.info("Sorting result")
          txCounts.zipWithIndex.sorted.takeRight(100).foreach {
            case (count, id) =>
              log.info(s"${db.get(Keys.idToAddress(AddressId(id.toLong)))}: $count")
          }
        case "ES" =>
          db.iterateOver(KeyTags.AddressScript) { e =>
            val asi = readAccountScriptInfo(e.getValue)
            val estimationResult = asi.script match {
              case ContractScript.ContractScriptImpl(stdLibVersion, expr) =>
                SetScriptTransactionDiff.estimate(reader, stdLibVersion, expr, checkOverflow = true)
              case script: ExprScript =>
                DiffsCommon.countVerifierComplexity(Some(script), reader, isAsset = false)
              case _ => ???
            }

            estimationResult.left.foreach { error =>
              val addressId = Longs.fromByteArray(e.getKey.drop(2).dropRight(4))
              val address   = db.get(Keys.idToAddress(AddressId(addressId)))
              log.info(s"$address: $error")
            }
          }

        case "CSAI" =>
          val PrefixLength = argument(1, "prefix").toInt
          var prevAssetId  = Array.emptyByteArray
          var assetCounter = 0
          db.iterateOver(KeyTags.AssetStaticInfo) { e =>
            assetCounter += 1
            val thisAssetId = e.getKey.drop(2)
            if (prevAssetId.nonEmpty) {
              var counter = 0
              while (counter < PrefixLength && prevAssetId(counter) == thisAssetId(counter)) counter += 1
              if (counter == PrefixLength) {
                log.info(s"${Base58.encode(prevAssetId)} ~ ${Base58.encode(thisAssetId)}")
              }
            }
            prevAssetId = thisAssetId
          }
          log.info(s"Checked $assetCounter asset(s)")
      }
    } finally db.close()
  }
}
