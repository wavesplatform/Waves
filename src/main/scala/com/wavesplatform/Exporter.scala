package com.wavesplatform

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.db.openDB
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.utils._
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.AddressScheme
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

object Exporter extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")
    val outputFilenamePrefix = Try(args(1)).toOption.getOrElse("blockchain")
    val exportHeight = Try(args(2)).toOption.flatMap(s => Try(s.toInt).toOption)

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    Try(openDB(settings.dataDirectory)) match {
      case Success(db) =>
        val tryHistory = HistoryWriterImpl(db, new ReentrantReadWriteLock(true), settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
        tryHistory match {
          case Success(history) =>
            val blockchainHeight = history.height()
            val height = Math.min(blockchainHeight, exportHeight.getOrElse(blockchainHeight))
            log.info(s"Blockchain height is $blockchainHeight exporting to $height")
            val outputFilename = s"$outputFilenamePrefix-$height"
            log.info(s"Output file: $outputFilename")

            createOutputStream(outputFilename) match {
              case Success(output) =>
                var exportedBytes = 0
                val bos = new BufferedOutputStream(output)
                val start = System.currentTimeMillis()
                (2 to height).foreach { h =>
                  val maybeBlockBytes = history.blockBytes(h)
                  maybeBlockBytes.foreach { bytes =>
                    val len = bytes.length
                    bos.write(Ints.toByteArray(len))
                    bos.write(bytes)
                    exportedBytes += len + Ints.BYTES
                    if (h % (height / 10) == 0)
                      log.info(s"$h blocks exported, ${humanReadableSize(exportedBytes)} written")
                  }
                }
                val duration = System.currentTimeMillis() - start
                log.info(s"Finished exporting $height blocks in ${humanReadableDuration(duration)}, ${humanReadableSize(exportedBytes)} written")
                bos.close()
                output.close()
              case Failure(ex) => log.error(s"Failed to create file '$outputFilename': $ex")
            }
          case Failure(ex) => log.error(s"Failed to open history at '${settings.dataDirectory}': $ex")
        }
      case Failure(ex) => log.error(s"Failed to open DB at '${settings.dataDirectory}': $ex")
    }
  }

  def createOutputStream(filename: String): Try[FileOutputStream] =
    Try {
      new FileOutputStream(filename)
    }
}
