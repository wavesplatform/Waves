package com.wavesplatform

import com.google.common.hash.{Funnels, BloomFilter as GBloomFilter}
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{AddressPortfolio, CommonAccountsApi}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.database.*
import com.wavesplatform.database.protobuf.StaticAssetInfo
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.{DiffsCommon, SetScriptTransactionDiff}
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{Blockchain, Height, Portfolio, StateSnapshot, TransactionId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging
import monix.execution.{ExecutionModel, Scheduler}
import org.rocksdb.{ReadOptions, RocksDB}
import play.api.libs.json.Json

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.annotation.tailrec
import scala.collection.immutable.VectorMap
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.*
import scala.util.Using

//noinspection ScalaStyle
object Explorer extends ScorexLogging {
  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  def portfolio(db: RocksDB, blockchain: Blockchain, address: Address): Portfolio =
    Portfolio(
      blockchain.balance(address),
      blockchain.leaseBalance(address),
      db.withResource(r => AddressPortfolio.assetBalanceIterator(r, address, StateSnapshot.empty, _ => true).flatten.to(VectorMap))
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

    val rdb    = RDB.open(settings.dbSettings)
    val reader = new RocksDBWriter(rdb, settings.blockchainSettings, settings.dbSettings, settings.enableLightMode)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    try {
      def loadBalanceHistory(curBalanceKey: Key[CurrentBalance], balanceNodeKey: Height => Key[BalanceNode]): Seq[(Int, Long)] = rdb.db.readOnly {
        db =>
          @tailrec
          def getPrevBalances(height: Height, acc: Seq[(Int, Long)]): Seq[(Int, Long)] = {
            if (height > 0) {
              val balance = rdb.db.get(balanceNodeKey(height))
              getPrevBalances(balance.prevHeight, (height, balance.balance) +: acc)
            } else acc
          }

          val currentBalance = db.get(curBalanceKey)
          (currentBalance.height, currentBalance.balance) +: getPrevBalances(currentBalance.prevHeight, Seq.empty).reverse
      }

      @inline
      def argument(i: Int, msg: => String) = args.applyOrElse(i, (_: Int) => throw new IllegalArgumentException(s"Argument #${i + 1} missing: $msg"))
      val flag                             = argument(0, "command").toUpperCase

      flag match {
        case "WB" =>
          var accountsBaseTotalBalance = 0L
          var wavesBalanceRecords      = 0
          rdb.db.iterateOver(KeyTags.WavesBalance) { e =>
            val addressId = AddressId(Longs.fromByteArray(e.getKey.drop(Shorts.BYTES)))
            val key       = Keys.wavesBalance(addressId)
            accountsBaseTotalBalance += key.parse(e.getValue).balance
            wavesBalanceRecords += 1
          }

          var actualTotalReward = 0L
          var blocksRecords     = 0
          rdb.db.iterateOver(KeyTags.BlockInfoAtHeight) { e =>
            val height = Height(Ints.fromByteArray(e.getKey.drop(Shorts.BYTES)))
            val key    = Keys.blockMetaAt(height)
            actualTotalReward += key.parse(e.getValue).fold(0L)(_.reward)
            blocksRecords += 1
          }

          log.info(s"Found $wavesBalanceRecords waves balance records and $blocksRecords block records")

          val actualTotalBalance   = accountsBaseTotalBalance + reader.carryFee(None)
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
          rdb.db.iterateOver(KeyTags.AddressId) { e =>
            val address   = Address.fromBytes(e.getKey.drop(2))
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
            val blockHeightBytes = rdb.db.get(kBlockHeight.keyBytes)
            val maybeBlockHeight = kBlockHeight.parse(blockHeightBytes)
            maybeBlockHeight.foreach { h =>
              val kBlock     = Keys.blockInfoBytesAt(Height(h))
              val blockBytes = rdb.db.get(kBlock.keyBytes)
              log.info(s"BlockId=${maybeBlockId.get} at h=$h: ${Base64.encode(blockBytes)}")
            }
          } else log.error("No block ID was provided")

        case "O" =>
          def loadVfHistory(orderId: ByteStr): Seq[(Int, Long, Long)] = {
            @tailrec
            def getPrevVfs(height: Height, acc: Seq[(Int, Long, Long)]): Seq[(Int, Long, Long)] = {
              if (height > 0) {
                val vf = rdb.db.get(Keys.filledVolumeAndFeeAt(orderId, height))
                getPrevVfs(vf.prevHeight, (height, vf.volume, vf.fee) +: acc)
              } else acc
            }

            val currentVf = rdb.db.get(Keys.filledVolumeAndFee(orderId))
            (currentVf.height, currentVf.volume, currentVf.fee) +: getPrevVfs(currentVf.prevHeight, Seq.empty).reverse
          }

          val orderId = Base58.tryDecodeWithLimit(argument(1, "order id")).toOption.map(ByteStr.apply)
          if (orderId.isDefined) {
            val kVolumeAndFee = Keys.filledVolumeAndFeeAt(orderId.get, Height(blockchainHeight))
            val bytes1        = rdb.db.get(kVolumeAndFee.keyBytes)
            val v             = kVolumeAndFee.parse(bytes1)
            log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: Volume = ${v.volume}, Fee = ${v.fee}")

            val vfHistory  = loadVfHistory(orderId.get)
            val heights    = vfHistory.map(_._1)
            val heightsStr = heights.mkString("[", ", ", "]")
            log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: History = $heightsStr")
            vfHistory.foreach { case (h, volume, fee) =>
              log.info(s"\t h = $h: Volume = $volume, Fee = $fee")
            }
          } else log.error("No order ID was provided")

        case "A" =>
          val address   = Address.fromString(argument(1, "address")).explicitGet()
          val aid       = Keys.addressId(address)
          val addressId = aid.parse(rdb.db.get(aid.keyBytes)).get
          log.info(s"Address id = $addressId")

          loadBalanceHistory(Keys.wavesBalance(addressId), Keys.wavesBalanceAt(addressId, _)).foreach { case (h, balance) =>
            log.info(s"h = $h: balance = $balance")
          }

        case "AC" =>
          val lastAddressId = Keys.lastAddressId.parse(rdb.db.get(Keys.lastAddressId.keyBytes))
          log.info(s"Last address id: $lastAddressId")

        case "AD" =>
          val result = new util.HashMap[Address, java.lang.Integer]()

          rdb.db.iterateOver(KeyTags.IdToAddress) { e =>
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
          val addressId = ai.parse(rdb.db.get(ai.keyBytes)).get
          log.info(s"Address ID = $addressId")

          loadBalanceHistory(Keys.assetBalance(addressId, asset), Keys.assetBalanceAt(addressId, asset, _)).foreach { case (h, balance) =>
            log.info(s"h = $h: balance = $balance")
          }

        case "S" =>
          log.info("Collecting DB stats")

          val result = new util.HashMap[Short, Stats]
          Seq(rdb.db.getDefaultColumnFamily, rdb.txHandle.handle, rdb.txSnapshotHandle.handle, rdb.txMetaHandle.handle).foreach { cf =>
            Using(new ReadOptions().setTotalOrderSeek(true)) { ro =>
              Using(rdb.db.newIterator(cf, ro)) { iterator =>
                iterator.seekToFirst()

                while (iterator.isValid) {
                  val keyPrefix   = ByteBuffer.wrap(iterator.key()).getShort
                  val valueLength = iterator.value().length
                  val keyLength   = iterator.key().length
                  result.compute(
                    keyPrefix,
                    (_, maybePrev) =>
                      maybePrev match {
                        case null => Stats(1, keyLength, valueLength)
                        case prev => Stats(prev.entryCount + 1, prev.totalKeySize + keyLength, prev.totalValueSize + valueLength)
                      }
                  )
                  iterator.next()
                }
              }
            }
          }

          log.info("key-space,entry-count,total-key-size,total-value-size")
          for ((prefix, stats) <- result.asScala) {
            log.info(s"${KeyTags(prefix)},${stats.entryCount},${stats.totalKeySize},${stats.totalValueSize}")
          }

        case "TXBH" =>
          val h   = Height(argument(1, "height").toInt)
          val txs = loadTransactions(h, rdb)

          println(txs.length)
          txs.foreach { case (_, tx) => println(tx) }

        case "AP" =>
          val address = Address.fromString(argument(1, "address")).explicitGet()
          val pf      = portfolio(rdb.db, reader, address)
          log.info(s"$address : ${pf.balance} WAVES, ${pf.lease}, ${pf.assets.size} assets")
          pf.assets.toSeq.sortBy(_._1.toString) foreach { case (assetId, balance) =>
            log.info(s"$assetId : $balance")
          }

        case "OC" =>
          log.info("Counting orders")
          var counter = 0L
          rdb.db.iterateOver(KeyTags.FilledVolumeAndFeeHistory) { _ =>
            counter += 1
          }
          log.info(s"Found $counter orders")

        case "CAT" =>
          log.info(s"Counting address transactions")
          val addressCount = rdb.db.get(Keys.lastAddressId).get.toInt
          log.info(s"Processing $addressCount addresses")
          val txCounts = new Array[Int](addressCount + 1)
          rdb.db.iterateOver(KeyTags.AddressTransactionHeightTypeAndNums) { e =>
            txCounts(Longs.fromByteArray(e.getKey.slice(2, 10)).toInt) += readTransactionHNSeqAndType(e.getValue)._2.size
          }
          log.info("Sorting result")
          txCounts.zipWithIndex.sorted.takeRight(100).foreach { case (count, id) =>
            log.info(s"${rdb.db.get(Keys.idToAddress(AddressId(id.toLong)))}: $count")
          }
        case "ES" =>
          rdb.db.iterateOver(KeyTags.AddressScript) { e =>
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
              val address   = rdb.db.get(Keys.idToAddress(AddressId(addressId)))
              log.info(s"$address: $error")
            }
          }

        case "CSAI" =>
          val PrefixLength = argument(1, "prefix").toInt
          var prevAssetId  = Array.emptyByteArray
          var assetCounter = 0
          rdb.db.iterateOver(KeyTags.AssetStaticInfo) { e =>
            assetCounter += 1
            val thisAssetId = StaticAssetInfo.parseFrom(e.getValue).id.toByteArray
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

        case "LDT" =>
          val s = Scheduler.fixedPool("foo-bar", 8, executionModel = ExecutionModel.AlwaysAsyncExecution)

          def countEntries(): Future[Long] = {
            CommonAccountsApi(() => SnapshotBlockchain(reader, StateSnapshot.empty), rdb, reader)
              .dataStream(Address.fromString("3PC9BfRwJWWiw9AREE2B3eWzCks3CYtg4yo").explicitGet(), None)
              .countL
              .runToFuture(s)
          }

          import scala.concurrent.ExecutionContext.Implicits.global

          println(
            Await.result(
              Future.sequence(Seq.fill(16)(countEntries())),
              Duration.Inf
            )
          )

        case "DDD" =>
          log.info(s"Collecting addresses")
          var count = 0L
          rdb.db.iterateOver(KeyTags.AddressId) { _ =>
            count += 1
          }
          log.info(s"Found $count addresses")
        case "TC" =>
          val bf = GBloomFilter.create[Array[Byte]](Funnels.byteArrayFunnel(), 200_000_000L)
          log.info("Counting transactions")
          var count = 0L
          rdb.db.iterateOver(KeyTags.TransactionMetaById, Some(rdb.txMetaHandle.handle)) { e =>
            bf.put(e.getKey.drop(2))
            count += 1
          }
          log.info(s"Found $count transactions")
        case "SH" =>
          val targetHeight = argument(1, "height").toInt
          log.info(s"Loading state hash at $targetHeight")
          rdb.db.get(Keys.stateHash(targetHeight)).foreach { sh =>
            println(Json.toJson(sh).toString())
          }
        case "CTI" =>
          log.info("Counting transaction IDs")
          var counter = 0
          Using(rdb.db.newIterator(rdb.txMetaHandle.handle)) { iter =>
            iter.seekToFirst()
//            iter.seek(KeyTags.TransactionMetaById.prefixBytes)
            log.info(iter.key().mkString(","))
            while (iter.isValid && iter.key().startsWith(KeyTags.TransactionMetaById.prefixBytes)) {
              counter += 1
              iter.next()
            }
          }
          log.info(s"Found $counter transaction IDs")
        case "TXM" =>
          log.info(s"TxMeta column family: ${new String(rdb.txMetaHandle.handle.getName)}/${rdb.txMetaHandle.handle.getID}")
          val id = argument(1, "id")
          log.info(s"Load meta for $id")
          val meta = rdb.db.get(Keys.transactionMetaById(TransactionId(ByteStr.decodeBase58(id).get), rdb.txMetaHandle))
          log.info(s"Meta: $meta")
      }
    } finally rdb.close()
  }
}
