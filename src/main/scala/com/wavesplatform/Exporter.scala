package com.wavesplatform

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.nio.charset.StandardCharsets

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.db.openDB
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils._
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.util.{Failure, Success, Try}

object Exporter extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename       = Try(args(0)).toOption.getOrElse("waves-testnet.conf")
    val outputFilenamePrefix = Try(args(1)).toOption.getOrElse("blockchain")
    val exportHeight         = Try(args(2)).toOption.flatMap(s => Try(s.toInt).toOption)
    val format               = Try(args(3)).toOption.filter(s => s.toUpperCase == "JSON").getOrElse("BINARY").toUpperCase

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val time             = new NTP(settings.ntpServer)
    val db               = openDB(settings.dataDirectory)
    val blockchain       = StorageFactory(settings, db, time)
    val blockchainHeight = blockchain.height
    val height           = Math.min(blockchainHeight, exportHeight.getOrElse(blockchainHeight))
    log.info(s"Blockchain height is $blockchainHeight exporting to $height")
    val outputFilename = s"$outputFilenamePrefix-$height"
    log.info(s"Output file: $outputFilename")

    createOutputStream(outputFilename) match {
      case Success(output) =>
        var exportedBytes = 0L
        val bos           = new BufferedOutputStream(output)
        val start         = System.currentTimeMillis()
        exportedBytes += writeHeader(bos, format)
        (2 to height).foreach { h =>
          exportedBytes += (if (format == "JSON") exportBlockToJson(bos, blockchain, h) else exportBlockToBinary(bos, blockchain, h))
          if (h % (height / 10) == 0)
            log.info(s"$h blocks exported, ${humanReadableSize(exportedBytes)} written")
        }
        exportedBytes += writeFooter(bos, format)
        val duration = System.currentTimeMillis() - start
        log.info(s"Finished exporting $height blocks in ${humanReadableDuration(duration)}, ${humanReadableSize(exportedBytes)} written")
        bos.close()
        output.close()
      case Failure(ex) => log.error(s"Failed to create file '$outputFilename': $ex")
    }

    time.close()
  }

  private def createOutputStream(filename: String): Try[FileOutputStream] =
    Try {
      new FileOutputStream(filename)
    }

  private def exportBlockToBinary(stream: OutputStream, blockchain: Blockchain, height: Int): Int = {
    val maybeBlockBytes = blockchain.blockBytes(height)
    maybeBlockBytes
      .map { bytes =>
        val len = bytes.length
        stream.write(Ints.toByteArray(len))
        stream.write(bytes)
        len + Ints.BYTES
      }
      .getOrElse(0)
  }

  private def exportBlockToJson(stream: OutputStream, blockchain: Blockchain, height: Int): Int = {
    val maybeBlock = blockchain.blockAt(height)
    maybeBlock
      .map { block =>
        val len = if (height != 2) {
          val bytes = ",\n".getBytes(StandardCharsets.UTF_8)
          stream.write(bytes)
          bytes.length
        } else 0
        val bytes = block.json().toString().getBytes(StandardCharsets.UTF_8)
        stream.write(bytes)
        len + bytes.length
      }
      .getOrElse(0)
  }

  private def writeHeader(stream: OutputStream, format: String): Int =
    if (format == "JSON") writeString(stream, "[\n") else 0

  private def writeFooter(stream: OutputStream, format: String): Int =
    if (format == "JSON") writeString(stream, "]\n") else 0

  private def writeString(stream: OutputStream, str: String): Int = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    stream.write(bytes)
    bytes.length
  }
}
