package com.wavesplatform.state2

import java.io.File

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.LevelDBFactory
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import org.iq80.leveldb.{DB, Options}
import scorex.account.{AddressScheme, Alias}
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration._
import scala.io.Codec
import scala.util.control.NonFatal

object RealDbBenchmark extends App with ScorexLogging {

  if (args.length < 1) {
    log.error("Specify a path to the node config")
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
  val db: DB = {
    val dir = new File(settings.dbPath)
    if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${settings.dbPath}'")
    LevelDBFactory.factory.open(dir, new Options)
  }

  try {
    val state = new LevelDBWriter(db, fs)

    if (!new File(settings.aliasesFile).isFile)
      throw new IllegalStateException(s"The file '${settings.aliasesFile}' does not exist, run ExtractInfo!")
    log.info(s"Loading aliases from '${settings.aliasesFile}'")
    val aliases: Vector[Alias] = scala.io.Source
      .fromFile(settings.aliasesFile)(Codec.UTF8)
      .getLines()
      .map(x => Alias.fromString(x).explicitGet())
      .toVector

    if (!new File(settings.restTxsFile).isFile)
      throw new IllegalStateException(s"The file '${settings.restTxsFile}' does not exist, run ExtractInfo!")
    log.info(s"Loading transactions from '${settings.restTxsFile}'")
    val allTxs: Vector[ByteStr] = scala.io.Source
      .fromFile(settings.restTxsFile)(Codec.UTF8)
      .getLines()
      .map(x => ByteStr.decodeBase58(x).toEither.explicitGet())
      .toVector

    log.info(s"Measuring resolveAlias")
    val aliasesStats = new mutable.ArrayBuffer[FiniteDuration]
    aliases.foreach { alias =>
      val start = System.currentTimeMillis()
      assert(state.resolveAlias(alias).isDefined)
      aliasesStats += (System.currentTimeMillis() - start).millis
    }
    logStats("resolveAlias", aliasesStats)

    log.info(s"Measuring transactionInfo")
    val transactionInfoStats = new mutable.ArrayBuffer[FiniteDuration]
    allTxs.foreach { txId =>
      val start = System.currentTimeMillis()
      assert(state.transactionInfo(txId).isDefined)
      transactionInfoStats += (System.currentTimeMillis() - start).millis
    }
    logStats("transactionInfo", transactionInfoStats)
  } catch {
    case NonFatal(e) => log.error(e.getMessage, e)
  } finally {
    db.close()
    log.info("Done")
  }

  def logStats(label: String, timing: mutable.ArrayBuffer[FiniteDuration]): Unit = {
    val total = timing.foldLeft(0.millis)(_ + _)
    val avg   = total / timing.size.toDouble
    val dev = timing.view.map { x =>
      math.pow(2, (x - avg).toMillis)
    }.sum
    val variance = dev / (timing.size - 1)
    log.info(
      f"""$label:
         |total:    ${total.toMillis}%5d ms = ${total.toNanos}%14d ns
         |average:  ${avg.toMillis}%5d ms = ${avg.toNanos}%14d ns
         |variance: $variance%3.5f""".stripMargin
    )
  }

}
