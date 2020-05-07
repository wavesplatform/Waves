package com.wavesplatform

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block
import com.wavesplatform.database.{DBExt, openDB}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.state.Height
import com.wavesplatform.utils._
import kamon.Kamon
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.iq80.leveldb.DB
import scopt.OParser

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object Exporter extends ScorexLogging {
  private[wavesplatform] object Formats {
    val Binary   = "BINARY"
    val Protobuf = "PROTOBUF"
    val Json     = "JSON"

    def list         = Seq(Binary, Protobuf, Json)
    def importerList = Seq(Binary, Protobuf)
    def default      = Binary

    def isSupported(f: String)           = list.contains(f.toUpperCase)
    def isSupportedInImporter(f: String) = importerList.contains(f.toUpperCase)
  }

  //noinspection ScalaStyle
  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, ExporterOptions()).foreach {
      case ExporterOptions(configFile, outputFileNamePrefix, exportHeight, format) =>
        implicit val reporter: UncaughtExceptionReporter = UncaughtExceptionReporter.default

        val settings = Application.loadApplicationConfig(Some(configFile))

        val time             = new NTP(settings.ntpServer)
        val db               = openDB(settings.dbSettings.directory)
        val (blockchain, _)  = StorageFactory(settings, db, time, Observer.empty, BlockchainUpdateTriggers.noop)
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
              exportedBytes += (if (format == "JSON") IO.exportBlockToJson(bos, db, h)
                                else IO.exportBlockToBinary(bos, db, h, format == Formats.Binary))
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

        Try(Await.result(Kamon.stopModules(), 10.seconds))
        Metrics.shutdown()
        time.close()
    }
  }

  private[this] object IO {
    def createOutputStream(filename: String): Try[FileOutputStream] =
      Try(new FileOutputStream(filename))

    def exportBlockToBinary(stream: OutputStream, db: DB, height: Int, legacy: Boolean): Int = {
      val maybeBlockBytes = db.readOnly(ro => database.loadBlock(Height(height), ro)).map(_.bytes())
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

    def exportBlockToJson(stream: OutputStream, db: DB, height: Int): Int = {
      val maybeBlock = db.readOnly(ro => database.loadBlock(Height(height), ro))
      maybeBlock
        .map { block =>
          val len = if (height != 2) {
            val bytes = ",\n".utf8Bytes
            stream.write(bytes)
            bytes.length
          } else 0
          val bytes = block.json().toString().utf8Bytes
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
      val bytes = str.utf8Bytes
      stream.write(bytes)
      bytes.length
    }
  }

  private[this] final case class ExporterOptions(
      configFileName: File = new File("waves-testnet.conf"),
      outputFileNamePrefix: String = "blockchain",
      exportHeight: Option[Int] = None,
      format: String = Formats.Binary
  )

  private[this] lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[ExporterOptions]
    import builder._

    OParser.sequence(
      programName("waves export"),
      head("Waves Blockchain Exporter", Version.VersionString),
      opt[File]('c', "config")
        .text("Node config file path")
        .action((f, c) => c.copy(configFileName = f)),
      opt[String]('o', "output-prefix")
        .text("Output file name prefix")
        .action((p, c) => c.copy(outputFileNamePrefix = p)),
      opt[String]('f', "format")
        .text("Output file format")
        .valueName(s"<${Formats.list.mkString("|")}> (default is ${Formats.default})")
        .action((f, c) => c.copy(format = f))
        .validate {
          case f if Formats.isSupported(f.toUpperCase) => success
          case f                                       => failure(s"Unsupported format: $f")
        },
      opt[Int]('h', "height")
        .text("Export to height")
        .action((h, c) => c.copy(exportHeight = Some(h)))
        .validate(h => if (h > 0) success else failure("Export height must be > 0")),
      help("help").hidden()
    )
  }
}
