package com.wavesplatform.state

import java.io.{File, PrintWriter}
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.ConfigFactory
import com.wavesplatform.Application
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{AddressId, DBExt, KeyTags, Keys, LevelDBFactory}
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.{DB, Options}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random
import scala.util.control.NonFatal

object ExtractInfo extends App with ScorexLogging {

  if (args.length < 1) {
    log.error("Specify a path to the node config. Usage: benchmark/run /full/path/to/the/config.conf")
    System.exit(1)
  }

  val maybeConfigFile = args.headOption.map(new File(_))
  val config          = loadConfig(maybeConfigFile.map(ConfigFactory.parseFile))
  val wavesSettings   = Application.loadApplicationConfig(maybeConfigFile)
  val benchSettings   = Settings.fromConfig(config)

  val db: DB = {
    val dir = new File(wavesSettings.dbSettings.directory)
    if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${wavesSettings.dbSettings.directory}'")
    LevelDBFactory.factory.open(dir, new Options)
  }

  try {
    log.info("Collecting aliases")

    val allAliases = Set.newBuilder[String]
    db.iterateOver(KeyTags.AddressIdOfAlias) { e =>
      allAliases += Alias.fromBytes(e.getKey.drop(2)).explicitGet().stringRepr
    }
    write("all aliases", benchSettings.aliasesFile, takeUniq(1000, allAliases.result().iterator))

    val maxAddressId = db.get(Keys.lastAddressId).getOrElse(0L).max(100)
    log.info(s"Collecting addresses, last address ID = $maxAddressId")
    val accountIds = randomInts(1, maxAddressId)
    val accounts   = accountIds.map(addressId => db.get(Keys.idToAddress(AddressId(addressId))).stringRepr)
    log.info("Saving accounts to file")
    write("accounts", benchSettings.accountsFile, takeUniq(1000, accounts))

    log.info("Collecting transactions")
    val txCountAtHeight = mutable.Map.empty[Int, Int].withDefault(h => db.get(Keys.blockMetaAt(Height(h))).fold(0)(_.transactionCount))
    val transactions = randomInts(1, db.get(Keys.height)).flatMap { h =>
      if (txCountAtHeight(h.toInt) <= 0) None
      else
        db.get(Keys.transactionAt(Height(h.toInt), TxNum(Random.nextInt(txCountAtHeight(h.toInt)).toShort))).map(_._1.id().toString)
    }

    write("rest transactions", benchSettings.restTxsFile, takeUniq(10000, transactions))

    log.info("Collecting assets")
    val allAssetIds = Set.newBuilder[String]
    db.iterateOver(KeyTags.AssetDetailsHistory) { e =>
      allAssetIds += ByteStr(e.getKey.drop(2)).toString
    }

    write("assets", benchSettings.assetsFile, takeUniq(300, allAssetIds.result().iterator))
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

  def randomInts(from: Long, to: Long): Iterator[java.lang.Long] =
    ThreadLocalRandom
      .current()
      .longs(from, to)
      .iterator()
      .asScala
}
