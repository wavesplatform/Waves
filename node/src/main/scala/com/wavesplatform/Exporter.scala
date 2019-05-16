package com.wavesplatform

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.nio.charset.StandardCharsets

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.block.Block
import com.wavesplatform.db.openDB
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils._
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import scopt.OParser

import scala.util.{Failure, Success, Try}

object Exporter extends ScorexLogging {
  //noinspection ScalaStyle
  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, ExporterOptions()).foreach {
      case ExporterOptions(configFilename, outputFileNamePrefix, exportHeight, format) =>
        val settings = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.parseFile(configFilename)))
        AddressScheme.current = new AddressScheme {
          override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
        }

        val time             = new NTP(settings.ntpServer)
        val db               = openDB(settings.dbSettings.directory)
        val blockchain       = StorageFactory(settings, db, time, Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr))
        val blockchainHeight = blockchain.height
        val height           = Math.min(blockchainHeight, exportHeight.getOrElse(blockchainHeight))
        log.info(s"Blockchain height is $blockchainHeight exporting to $height")
        val outputFilename = s"$outputFileNamePrefix-$height"
        log.info(s"Output file: $outputFilename")

        IO.createOutputStream(outputFilename) match {
          case Success(output) =>
            var exportedBytes = 0L
            val bos           = new BufferedOutputStream(output)
            val start         = System.currentTimeMillis()
            exportedBytes += IO.writeHeader(bos, format)
            (2 to height).foreach { h =>
              exportedBytes += (if (format == "JSON") IO.exportBlockToJson(bos, blockchain, h)
                                else IO.exportBlockToBinary(bos, blockchain, h, format == "BINARY_OLD"))
              if (h % (height / 10) == 0)
                log.info(s"$h blocks exported, ${humanReadableSize(exportedBytes)} written")
            }
            exportedBytes += IO.writeFooter(bos, format)
            val duration = System.currentTimeMillis() - start
            log.info(s"Finished exporting $height blocks in ${humanReadableDuration(duration)}, ${humanReadableSize(exportedBytes)} written")
            bos.close()
            output.close()
          case Failure(ex) => log.error(s"Failed to create file '$outputFilename': $ex")
        }

        time.close()
    }
  }

  private[this] object IO {
    def createOutputStream(filename: String): Try[FileOutputStream] =
      Try(new FileOutputStream(filename))

    def exportBlockToBinary(stream: OutputStream, blockchain: Blockchain, height: Int, legacy: Boolean): Int = {
      val maybeBlockBytes = blockchain.blockBytes(height)
      maybeBlockBytes
        .map { oldBytes =>
          val bytes       = if (legacy) oldBytes else PBBlocks.clearChainId(PBBlocks.protobuf(Block.parseBytes(oldBytes).get)).toByteArray
          val bytesLength = bytes.length

          stream.write(Ints.toByteArray(bytesLength))
          stream.write(bytes)

          Ints.BYTES + bytesLength
        }
        .getOrElse(0)
    }

    def exportBlockToJson(stream: OutputStream, blockchain: Blockchain, height: Int): Int = {
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

    def writeHeader(stream: OutputStream, format: String): Int =
      if (format == "JSON") writeString(stream, "[\n") else 0

    def writeFooter(stream: OutputStream, format: String): Int =
      if (format == "JSON") writeString(stream, "]\n") else 0

    def writeString(stream: OutputStream, str: String): Int = {
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      stream.write(bytes)
      bytes.length
    }
  }

  private[this] final case class ExporterOptions(configFileName: File = new File("waves-testnet.conf"),
                                                 outputFileNamePrefix: String = "blockchain",
                                                 exportHeight: Option[Int] = None,
                                                 format: String = "BINARY_OLD")

  private[this] lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[ExporterOptions]
    import builder._

    OParser.sequence(
      programName("waves export"),
      head("Waves Blockchain Exporter", Version.VersionString),
      opt[File]('c', "config")
        .text("Config file name")
        .action((f, c) => c.copy(configFileName = f)),
      opt[String]("output-prefix")
        .abbr("op")
        .text("Output file name prefix")
        .action((p, c) => c.copy(outputFileNamePrefix = p)),
      opt[String]('f', "format")
        .text("Output file format")
        .valueName("<BINARY|BINARY_OLD|JSON>")
        .action((f, c) => c.copy(format = f))
        .validate {
          case f if Set("BINARY", "BINARY_OLD", "JSON").contains(f.toUpperCase) => success
          case f                                                                => failure(s"Unsupported format: $f")
        }
    )
  }
}
