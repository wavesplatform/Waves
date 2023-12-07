package com.wavesplatform

import com.google.common.collect.AbstractIterator

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import com.google.common.primitives.Ints
import com.wavesplatform.block.Block
import com.wavesplatform.database.protobuf.BlockMeta
import com.wavesplatform.database.{KeyTags, RDB, createBlock, readBlockMeta, readTransaction}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.*
import kamon.Kamon
import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB}
import scopt.OParser

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}

object Exporter extends ScorexLogging {
  private[wavesplatform] object Formats {
    val Binary   = "BINARY"
    val Protobuf = "PROTOBUF"

    def list: Seq[String] = Seq(Binary, Protobuf)
    def default: String   = Binary

    def isSupported(f: String): Boolean = list.contains(f.toUpperCase)
  }

  // noinspection ScalaStyle
  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, ExporterOptions()).foreach {
      case ExporterOptions(configFile, blocksOutputFileNamePrefix, snapshotsOutputFileNamePrefix, exportSnapshots, exportHeight, format) =>
        val settings = Application.loadApplicationConfig(configFile)

        Using.resources(
          new NTP(settings.ntpServer),
          RDB.open(settings.dbSettings)
        ) { (time, rdb) =>
          val (blockchain, _)  = StorageFactory(settings, rdb, time, BlockchainUpdateTriggers.noop)
          val blockchainHeight = blockchain.height
          val height           = Math.min(blockchainHeight, exportHeight.getOrElse(blockchainHeight))
          log.info(s"Blockchain height is $blockchainHeight exporting to $height")
          val blocksOutputFilename = s"$blocksOutputFileNamePrefix-$height"
          log.info(s"Blocks output file: $blocksOutputFilename")

          val snapshotsOutputFilename = if (exportSnapshots) {
            val filename = s"$snapshotsOutputFileNamePrefix-$height"
            log.info(s"Snapshots output file: $filename")
            Some(filename)
          } else None

          implicit def optReleasable[A](implicit ev: Releasable[A]): Releasable[Option[A]] = {
            case Some(r) => ev.release(r)
            case None    => ()
          }

          Using.resources(
            createOutputFile(blocksOutputFilename),
            snapshotsOutputFilename.map(createOutputFile)
          ) { case (blocksOutput, snapshotsOutput) =>
            Using.resources(createBufferedOutputStream(blocksOutput, 10), snapshotsOutput.map(createBufferedOutputStream(_, 100))) {
              case (blocksStream, snapshotsStream) =>
                var exportedBlocksBytes    = 0L
                var exportedSnapshotsBytes = 0L
                val start                  = System.currentTimeMillis()

                new BlockSnapshotIterator(rdb, height, exportSnapshots).asScala.foreach { case (h, block, txSnapshots) =>
                  exportedBlocksBytes += IO.exportBlock(blocksStream, Some(block), format == Formats.Binary)
                  snapshotsStream.foreach { output =>
                    exportedSnapshotsBytes += IO.exportBlockTxSnapshots(output, txSnapshots)
                  }

                  if (h % (height / 10) == 0) {
                    log.info(
                      s"$h blocks exported, ${humanReadableSize(exportedBlocksBytes)} written for blocks${snapshotsLogInfo(exportSnapshots, exportedSnapshotsBytes)}"
                    )
                  }
                }
                val duration = System.currentTimeMillis() - start
                log
                  .info(
                    s"Finished exporting $height blocks in ${java.time.Duration.ofMillis(duration)}, ${humanReadableSize(exportedBlocksBytes)} written for blocks${snapshotsLogInfo(exportSnapshots, exportedSnapshotsBytes)}"
                  )
            }
          }
        }

        Try(Await.result(Kamon.stopModules(), 10.seconds))
        Metrics.shutdown()
    }
  }

  private class BlockSnapshotIterator(rdb: RDB, targetHeight: Int, exportSnapshots: Boolean) extends AbstractIterator[(Int, Block, Seq[Array[Byte]])] {
    var nextTxEntry: Option[(Int, Transaction)]       = None
    var nextSnapshotEntry: Option[(Int, Array[Byte])] = None

    val blockMetaIterator: DataIterator[BlockMeta] =
      new DataIterator[BlockMeta](
        rdb.db,
        rdb.db.getDefaultColumnFamily,
        KeyTags.BlockInfoAtHeight.prefixBytes,
        _.takeRight(Ints.BYTES),
        _ => readBlockMeta
      )
    val txIterator: DataIterator[Transaction] = {
      val prefixBytes = KeyTags.NthTransactionInfoAtHeight.prefixBytes
      new DataIterator(
        rdb.db,
        rdb.txHandle.handle,
        prefixBytes,
        _.slice(prefixBytes.length, prefixBytes.length + Ints.BYTES),
        h => readTransaction(Height(h))(_)._2
      )
    }
    val snapshotIterator: DataIterator[Array[Byte]] = {
      val prefixBytes = KeyTags.NthTransactionStateSnapshotAtHeight.prefixBytes
      new DataIterator(
        rdb.db,
        rdb.txSnapshotHandle.handle,
        prefixBytes,
        _.slice(prefixBytes.length, prefixBytes.length + Ints.BYTES),
        _ => identity
      )
    }

    def loadTxData[A](acc: Seq[A], height: Int, iterator: DataIterator[A], updateNextEntryF: (Int, A) => Unit): Seq[A] = {
      if (iterator.hasNext) {
        val (h, txData) = iterator.next()
        if (h == height) {
          loadTxData(txData +: acc, height, iterator, updateNextEntryF)
        } else {
          updateNextEntryF(h, txData)
          acc.reverse
        }
      } else acc.reverse
    }

    override def computeNext(): (Int, Block, Seq[Array[Byte]]) = {
      if (blockMetaIterator.hasNext) {
        val (h, meta) = blockMetaIterator.next()
        if (h <= targetHeight) {
          val txs = nextTxEntry match {
            case Some((txHeight, tx)) if txHeight == h =>
              nextTxEntry = None
              loadTxData[Transaction](Seq(tx), h, txIterator, (h, tx) => nextTxEntry = Some(h -> tx))
            case Some(_) => Seq.empty
            case _       => loadTxData[Transaction](Seq.empty, h, txIterator, (h, tx) => nextTxEntry = Some(h -> tx))
          }
          val snapshots = if (exportSnapshots) {
            nextSnapshotEntry match {
              case Some((snapshotHeight, txSnapshot)) if snapshotHeight == h =>
                nextSnapshotEntry = None
                loadTxData[Array[Byte]](Seq(txSnapshot), h, snapshotIterator, (h, sn) => nextSnapshotEntry = Some(h -> sn))
              case Some(_) => Seq.empty
              case _       => loadTxData[Array[Byte]](Seq.empty, h, snapshotIterator, (h, sn) => nextSnapshotEntry = Some(h -> sn))
            }
          } else Seq.empty
          createBlock(PBBlocks.vanilla(meta.getHeader), meta.signature.toByteStr, txs).toOption
            .map(block => (h, block, snapshots))
            .getOrElse(computeNext())
        } else {
          closeResources()
          endOfData()
        }
      } else {
        closeResources()
        endOfData()
      }
    }

    def closeResources(): Unit = {
      txIterator.closeResources()
      snapshotIterator.closeResources()
      blockMetaIterator.closeResources()
    }
  }

  private class DataIterator[A](
      db: RocksDB,
      cfHandle: ColumnFamilyHandle,
      prefixBytes: Array[Byte],
      heightFromKeyF: Array[Byte] => Array[Byte],
      parseDataF: Int => Array[Byte] => A
  ) extends AbstractIterator[(Int, A)] {
    private val snapshot    = db.getSnapshot
    private val readOptions = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)
    private val dbIterator  = db.newIterator(cfHandle, readOptions.setTotalOrderSeek(true))

    dbIterator.seek(prefixBytes)

    override def computeNext(): (Int, A) = {
      if (dbIterator.isValid && dbIterator.key().startsWith(prefixBytes)) {
        val h = Ints.fromByteArray(heightFromKeyF(dbIterator.key()))
        if (h > 1) {
          val txData = parseDataF(h)(dbIterator.value())
          dbIterator.next()
          h -> txData
        } else {
          dbIterator.next()
          computeNext()
        }
      } else {
        closeResources()
        endOfData()
      }
    }

    def closeResources(): Unit = {
      snapshot.close()
      readOptions.close()
      dbIterator.close()
    }
  }

  object IO {
    def createOutputStream(filename: String): Try[FileOutputStream] =
      Try(new FileOutputStream(filename))

    def exportBlock(stream: OutputStream, maybeBlock: Option[Block], legacy: Boolean): Int = {
      val maybeBlockBytes = maybeBlock.map(_.bytes())
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

    def exportBlockTxSnapshots(stream: OutputStream, snapshots: Seq[Array[Byte]]): Int = {
      val snapshotBytesWithSizes = snapshots.map { snapshot =>
        snapshot -> snapshot.length
      }

      val fullSize = snapshotBytesWithSizes.map(_._2 + Ints.BYTES).sum
      stream.write(Ints.toByteArray(fullSize))

      snapshotBytesWithSizes.foreach { case (snapshotBytes, size) =>
        stream.write(Ints.toByteArray(size))
        stream.write(snapshotBytes)
      }

      fullSize + Ints.BYTES
    }

    def writeString(stream: OutputStream, str: String): Int = {
      val bytes = str.utf8Bytes
      stream.write(bytes)
      bytes.length
    }
  }

  private[this] final case class ExporterOptions(
      configFileName: Option[File] = None,
      blocksOutputFileNamePrefix: String = "blockchain",
      snapshotsFileNamePrefix: String = "snapshots",
      exportSnapshots: Boolean = false,
      exportHeight: Option[Int] = None,
      format: String = Formats.Binary
  )

  private[this] lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[ExporterOptions]
    import builder.*

    OParser.sequence(
      programName("waves export"),
      head("Waves Blockchain Exporter", Version.VersionString),
      opt[File]('c', "config")
        .text("Node config file path")
        .action((f, c) => c.copy(configFileName = Some(f))),
      opt[String]('o', "output-prefix")
        .text("Blocks output file name prefix")
        .action((p, c) => c.copy(blocksOutputFileNamePrefix = p)),
      opt[String]('s', "snapshot-output-prefix")
        .text("Snapshots output file name prefix")
        .action((p, c) => c.copy(snapshotsFileNamePrefix = p)),
      opt[Unit]('l', "export-snapshots")
        .text("Export snapshots for light node")
        .action((_, c) => c.copy(exportSnapshots = true)),
      opt[Int]('h', "height")
        .text("Export to height")
        .action((h, c) => c.copy(exportHeight = Some(h)))
        .validate(h => if (h > 0) success else failure("Export height must be > 0")),
      opt[String]('f', "format")
        .hidden()
        .text("Output file format")
        .valueName(s"<${Formats.list.mkString("|")}> (default is ${Formats.default})")
        .action { (f, c) =>
          log.warn("Export file format option is deprecated and will be removed eventually")
          c.copy(format = f)
        }
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

  private def createOutputFile(outputFilename: String): FileOutputStream =
    IO.createOutputStream(outputFilename) match {
      case Success(output) => output
      case Failure(ex) =>
        log.error(s"Failed to create file '$outputFilename': $ex")
        throw ex
    }

  private def createBufferedOutputStream(fileOutputStream: FileOutputStream, sizeInMb: Int) =
    new BufferedOutputStream(fileOutputStream, sizeInMb * 1024 * 1024)

  private def snapshotsLogInfo(exportSnapshots: Boolean, exportedSnapshotsBytes: Long): String =
    if (exportSnapshots) {
      s", ${humanReadableSize(exportedSnapshotsBytes)} for snapshots"
    } else ""
}
