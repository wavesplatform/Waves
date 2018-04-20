package com.wavesplatform.state2

import java.io.File
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.LevelDBFactory
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state2.bench.DataTestData
import monix.eval.Coeval
import org.iq80.leveldb.{DB, Options}
import scodec.bits.BitVector
import scorex.account.{AddressOrAlias, AddressScheme, Alias}
import scorex.crypto.encode.Base58
import scorex.transaction.smart.WavesEnvironment
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration._
import scala.io.Codec
import scala.util.control.NonFatal

/**
  * Tests over real database. How to test:
  * 1. Download a database
  * 2. Import it: https://github.com/wavesplatform/Waves/wiki/Export-and-import-of-the-blockchain#import-blocks-from-the-binary-file
  * 3. Run ExtractInfo to collect queries for tests
  * 4. Run this test
  */
object RealDbBenchmark extends App with ScorexLogging {

  if (args.length < 1) {
    log.error("Specify a path to the node config. Usage: benchmark/run /full/path/to/the/config.conf")
    System.exit(1)
  }

  val config = loadConfig(ConfigFactory.parseFile(new File(args.head)))

  val fs: FunctionalitySettings = {
    val settings = WavesSettings.fromConfig(config)
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }
    settings.blockchainSettings.functionalitySettings
  }

  val settings = Settings.fromConfig(config)
  Seq(
    settings.aliasesFile,
    settings.restTxsFile,
    settings.accountsFile,
    settings.assetsFile
  ).foreach { f =>
    if (!new File(f).isFile)
      throw new IllegalStateException(s"The file '$f' does not exist, run ExtractInfo!")
  }

  {
    val aliases = load("resolveAddress", settings.aliasesFile)(x => Alias.fromString(x).explicitGet())
    measure("resolveAddress, resolveAlias (WavesContext.addressFromRecipient)", aliases) { (environment, alias) =>
      assert(environment.resolveAddress(alias.bytes.arr).isRight)
    }
  }

  {
    val allTxs = load("transactionById", settings.restTxsFile)(x => Base58.decode(x).get)
    measure("transactionById (WavesContext.getTransactionById)", allTxs) { (environment, txId) =>
      assert(environment.transactionById(txId).isDefined)
    }
    measure("transactionHeightById (WavesContext.transactionHeightById)", allTxs) { (environment, txId) =>
      assert(environment.transactionHeightById(txId).isDefined)
    }
  }

  val accounts = load("accounts", settings.accountsFile)(x => AddressOrAlias.fromString(x).explicitGet())
  measure("accountBalance (WavesContext.accountBalance)", accounts) { (environment, account) =>
    assert(environment.accountBalanceOf(account.bytes.arr, None).isRight)
  }

  val assets = load("assets", settings.assetsFile)(x => Base58.decode(x).get)
  val continuallyAssets = Iterator.continually {
    assets(ThreadLocalRandom.current().nextInt(assets.length))
  }

  val assetsTestInput: Iterator[(Array[Byte], AddressOrAlias)] = continuallyAssets
    .zip(accounts.toIterator)
    .take(math.min(3000, accounts.size))
  measure("accountBalanceOf (WavesContext.accountAssetBalance)", assetsTestInput) {
    case (environment, (asset, account)) =>
      assert(environment.accountBalanceOf(account.bytes.arr, Some(asset)).isRight)
  }

  {
    val data = load("data", settings.dataFile) { line =>
      DataTestData.codec.decode(BitVector.fromBase64(line).get).require.value
    }
    measure("data (WavesContext.getData)", data) { (environment, x) =>
      assert(environment.data(x.addr.toArray, x.key, x.dataType).isDefined)
    }
  }

  log.info("Done")

  def load[T](label: String, absolutePath: String)(f: String => T): Vector[T] = {
    log.info(s"Loading $label from '$absolutePath'")
    scala.io.Source
      .fromFile(absolutePath)(Codec.UTF8)
      .getLines()
      .map(f)
      .toVector
  }

  def measure[T](label: String, input: TraversableOnce[T])(f: (Environment, T) => Any): Unit = {
    log.info("Opening the DB")
    val db: DB = {
      val dir = new File(settings.dbPath)
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${settings.dbPath}'")
      LevelDBFactory.factory.open(dir, new Options)
    }

    try {
      val state = new LevelDBWriter(db, fs)

      val environment = new WavesEnvironment(
        AddressScheme.current.chainId,
        Coeval.raiseError(new NotImplementedError("tx is not implemented")),
        Coeval(state.height),
        state
      )

      log.info(s"Measuring $label...")
      val stats = new mutable.ArrayBuffer[FiniteDuration]
      input.foreach { x =>
        val start = System.nanoTime()
        try {
          f(environment, x)
        } catch {
          case _: Throwable =>
        }
        stats += (System.nanoTime() - start).nanos
      }
      logStats(label, stats)
    } catch {
      case NonFatal(e) => log.error(e.getMessage, e)
    } finally {
      db.close()
      log.info(s"Measuring $label is done")
      System.gc()
    }
  }

  def logStats(label: String, timing: mutable.ArrayBuffer[FiniteDuration]): Unit = {
    val avg = timing.view.foldLeft(BigDecimal(0))((r, x) => r + BigDecimal(x.toNanos) / timing.size)
    log.info(
      f"""$label:
         |average:  ${(avg / BigDecimal(10).pow(6)).toLong}%5d ms = ${avg.toLong}%14d ns""".stripMargin
    )
  }

}
