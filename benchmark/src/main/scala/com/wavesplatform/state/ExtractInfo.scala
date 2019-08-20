package com.wavesplatform.state

import java.io.{File, PrintWriter}
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.block.Block
import com.wavesplatform.database.{LevelDBFactory, LevelDBWriter}
import com.wavesplatform.lang.v1.traits.DataType
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.bench.DataTestData
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Authorized, CreateAliasTransactionV1, DataTransaction, Transaction}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.iq80.leveldb.{DB, Options}
import scodec.bits.BitVector

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Extracts data from the database to use it in RealDbBenchmark.
  * Requires a separate main file because takes too long time to run.
  */
object ExtractInfo extends App with ScorexLogging {

  if (args.length < 1) {
    log.error("Specify a path to the node config. Usage: benchmark/run /full/path/to/the/config.conf")
    System.exit(1)
  }

  val benchSettings = Settings.fromConfig(ConfigFactory.load())
  val wavesSettings = {
    val config = loadConfig(ConfigFactory.parseFile(new File(args.head)))
    WavesSettings.fromRootConfig(config)
  }

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = wavesSettings.blockchainSettings.addressSchemeCharacter.toByte
  }

  val db: DB = {
    val dir = new File(wavesSettings.dbSettings.directory)
    if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${wavesSettings.dbSettings.directory}'")
    LevelDBFactory.factory.open(dir, new Options)
  }

  try {
    val state = new LevelDBWriter(db,
                                  Observer.empty(UncaughtExceptionReporter.default),
                                  wavesSettings.blockchainSettings.functionalitySettings,
                                  wavesSettings.dbSettings)

    def nonEmptyBlockHeights(from: Int): Iterator[Integer] =
      for {
        height     <- randomInts(from, state.height)
        (block, _) <- state.blockHeaderAndSize(height)
        if block.transactionCount > 0
      } yield height

    def nonEmptyBlocks(from: Int): Iterator[Block] =
      nonEmptyBlockHeights(from)
        .flatMap(state.blockAt(_))

    val aliasTxs = nonEmptyBlocks(benchSettings.aliasesFromHeight)
      .flatMap(_.transactionData)
      .collect {
        case _: CreateAliasTransactionV1 => true
      }

    val restTxs = nonEmptyBlocks(benchSettings.restTxsFromHeight)
      .flatMap(_.transactionData)

    val accounts = for {
      b <- nonEmptyBlocks(benchSettings.accountsFromHeight)
      sender <- b.transactionData
        .collect {
          case tx: Transaction with Authorized => tx.sender
        }
        .take(100)
    } yield sender.toAddress.stringRepr
    write("accounts", benchSettings.accountsFile, takeUniq(1000, accounts))

    val aliasTxIds = aliasTxs.map(_.asInstanceOf[CreateAliasTransactionV1].alias.stringRepr)
    write("aliases", benchSettings.aliasesFile, aliasTxIds.take(1000))

    val restTxIds = restTxs.map(_.id().base58)
    write("rest transactions", benchSettings.restTxsFile, restTxIds.take(10000))

    val assets = nonEmptyBlocks(benchSettings.assetsFromHeight)
      .flatMap { b =>
        b.transactionData.collect {
          case tx: IssueTransaction => tx.assetId
        }
      }
      .map(_.base58)

    write("assets", benchSettings.assetsFile, takeUniq(300, assets))

    val data = for {
      b <- nonEmptyBlocks(benchSettings.dataFromHeight)
      test <- b.transactionData
        .collect {
          case tx: DataTransaction =>
            val addr = tx.sender.toAddress.bytes
            tx.data.collectFirst {
              case x: IntegerDataEntry => DataTestData(addr, x.key, DataType.Long)
              case x: BooleanDataEntry => DataTestData(addr, x.key, DataType.Boolean)
              case x: BinaryDataEntry  => DataTestData(addr, x.key, DataType.ByteArray)
              case x: StringDataEntry  => DataTestData(addr, x.key, DataType.String)
            }
        }
        .take(50)
      r <- test
    } yield {
      val x: BitVector = DataTestData.codec.encode(r).require
      x.toBase64
    }
    write("data", benchSettings.dataFile, data.take(400))
  } catch {
    case NonFatal(e) => log.error(e.getMessage, e)
  } finally {
    db.close()
    log.info("Done")
  }

  def takeUniq[T](size: Int, xs: Iterator[T]): mutable.Set[T] = {
    val r = mutable.Set.empty[T]
    xs.find { x =>
      r.add(x)
      r.size == size
    }
    r
  }

  def write(label: String, absolutePath: String, data: TraversableOnce[String]): Unit = {
    log.info(s"Writing $label to '$absolutePath'")
    val printWriter = new PrintWriter(absolutePath)
    data.foreach(printWriter.println)
    printWriter.close()
  }

  def randomInts(from: Int, to: Int): Iterator[Integer] =
    ThreadLocalRandom
      .current()
      .ints(from, to)
      .iterator()
      .asScala

}
