package com.wavesplatform.state2

import java.io.{File, PrintWriter}
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.LevelDBFactory
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import org.iq80.leveldb.{DB, Options}
import scorex.account.AddressScheme
import scorex.transaction.CreateAliasTransaction
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
  * Extracts data from the database to use it in RealDbBenchmark.
  * Requires a separate main file because takes too long time to run.
  */
object ExtractInfo extends App with ScorexLogging {

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

    val nonEmptyBlockHeights: Iterator[Integer] = for {
      height <- ThreadLocalRandom
        .current()
        .ints(2, state.height)
        .iterator()
        .asScala
      (block, _) <- state.blockHeaderAndSize(height)
      if block.transactionCount > 0
    } yield height

    val (aliasTxs, restTxs) = nonEmptyBlockHeights
      .flatMap(state.blockAt(_))
      .flatMap(_.transactionData)
      .partition {
        case _: CreateAliasTransaction => true
        case _                         => false
      }

    val aliasTxIds = aliasTxs
      .map(_.asInstanceOf[CreateAliasTransaction].alias.stringRepr)
      .take(5000)

    val restTxIds = restTxs
      .map(_.id().base58)
      .take(10000)

    log.info(s"Writing aliases to '${settings.aliasesFile}'")
    val aliasTxsFile = new PrintWriter(settings.aliasesFile)
    aliasTxIds.foreach(aliasTxsFile.println)
    aliasTxsFile.close()

    log.info(s"Writing rest transactions to '${settings.restTxsFile}'")
    val restTxsFile = new PrintWriter(settings.restTxsFile)
    restTxIds.foreach(restTxsFile.println)
    restTxsFile.close()
  } catch {
    case NonFatal(e) => log.error(e.getMessage, e)
  } finally {
    db.close()
    log.info("Done")
  }

}
