package com.wavesplatform

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.{Bytes, Ints}
import com.google.protobuf.{ByteString, CodedInputStream}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.block.serialization.{BlockSerializer, mkTxsCountBytes, writeTransactionData}
import com.wavesplatform.database.protobuf.BlockMeta
import com.wavesplatform.database.{Caches, KeyTag, RDB, createBlock, readBlockMeta, readTransaction}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks.protobuf
import com.wavesplatform.protobuf.block.{PBBlock, PBBlocks}
import com.wavesplatform.protobuf.transaction.{PBTransactions, SignedTransaction}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.{Transaction, TransactionParsers}
import com.wavesplatform.utils.*
import kamon.Kamon
import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB}
import scopt.OParser

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}

object Exporter extends ScorexLogging {
  // noinspection ScalaStyle
  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, ExporterOptions()).foreach {
      case ExporterOptions(configFile, blocksOutputFileNamePrefix, snapshotsOutputFileNamePrefix, exportHeight) =>
        val settings = Application.loadApplicationConfig(configFile)

        Using.resources(
          new NTP(settings.ntpServer),
          RDB.open(settings.dbSettings)
        ) { (time, rdb) =>
          val (blockchain, rdbWriter) = StorageFactory(settings, rdb, time, BlockchainUpdateTriggers.noop)
          val blockchainHeight        = blockchain.height
          val height                  = Math.min(blockchainHeight, exportHeight.getOrElse(blockchainHeight))
          log.info(s"Blockchain height is $blockchainHeight exporting to $height")
          val blocksOutputFilename = s"$blocksOutputFileNamePrefix-$height"
          log.info(s"Blocks output file: $blocksOutputFilename")

          val exportSnapshots = snapshotsOutputFileNamePrefix.isDefined
          val snapshotsOutputFilename = if (exportSnapshots) {
            val filename = s"${snapshotsOutputFileNamePrefix.get}-$height"
            log.info(s"Snapshots output file: $filename")
            Some(filename)
          } else None

          implicit def optReleasable[A](implicit ev: Releasable[A]): Releasable[Option[A]] = {
            case Some(r) => ev.release(r)
            case None    => ()
          }

          Using.resources(
            createOutputFile(blocksOutputFilename),
            snapshotsOutputFilename.map(createOutputFile),
            rdbWriter
          ) { case (blocksOutput, snapshotsOutput, _) =>
            Using.resources(createBufferedOutputStream(blocksOutput, 10), snapshotsOutput.map(createBufferedOutputStream(_, 100))) {
              case (blocksStream, snapshotsStream) =>
                var exportedBlocksBytes    = 0L
                var exportedSnapshotsBytes = 0L
                val start                  = System.currentTimeMillis()

                val txIterator = new DataIterator[ByteString | SignedTransaction](
                  rdb.db, rdb.txHandle.handle, KeyTag.NthTransactionInfoAtHeight.prefixBytes, _.slice(2, 6),
                  height => bytes => readTx(CodedInputStream.newInstance(bytes))
                )

                var counter = 0

                val blockMetaIterator: DataIterator[BlockMeta] =
                  new DataIterator[BlockMeta](
                    rdb.db,
                    rdb.db.getDefaultColumnFamily,
                    KeyTag.BlockInfoAtHeight.prefixBytes,
                    _.takeRight(Ints.BYTES),
                    _ => readBlockMeta
                  )

                while (blockMetaIterator.hasNext) {
                  val meta = blockMetaIterator.next()._2
                  val txCount = meta.transactionCount
                  val txs = txIterator.asScala.take(txCount).toSeq
                  val signedHeader = Caches.toSignedHeader(meta)
                  val blockBytes = if (signedHeader.header.version >= Block.ProtoBlockVersion)
                    PBUtils.encodeDeterministic(new PBBlock(
                      Some(protobuf(signedHeader.header)),
                      ByteString.copyFrom(signedHeader.signature.arr),
                      txs.map(_._2 match {
                        case s: SignedTransaction => s
                        case bs: ByteString => PBTransactions.protobuf(TransactionParsers.parseBytes(bs.toByteArray).get)
                      }).toSeq
                    ))
                  else {
                    Bytes.concat(
                      BlockSerializer.mkPrefixBytes(signedHeader.header),
                      mkTxsDataBytes(signedHeader.header, txs.map(_._2.asInstanceOf[ByteString].toByteArray).toSeq),
                      BlockSerializer.mkSuffixBytes(signedHeader.header, signedHeader.signature)
                    )
                  }
                  blocksStream.write(Ints.toByteArray(blockBytes.length))
                  blocksStream.write(blockBytes)
                  exportedBlocksBytes += blockBytes.length

                  if ((counter + txCount) / 1_000_000 - (counter / 1_000_000) > 0) {
                    log.info(f"Exported ${counter + txCount}%,d transactions, written $exportedBlocksBytes%,d bytes")
                  }
                  counter += txCount
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

    @tailrec
    override final def computeNext(): (Int, A) = {
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
  }

  private final case class ExporterOptions(
      configFileName: Option[File] = None,
      blocksOutputFileNamePrefix: String = "blockchain",
      snapshotsFileNamePrefix: Option[String] = None,
      exportHeight: Option[Int] = None
  )

  private lazy val commandParser = {
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
        .action((p, c) => c.copy(snapshotsFileNamePrefix = Some(p))),
      opt[Int]('h', "height")
        .text("Export to height")
        .action((h, c) => c.copy(exportHeight = Some(h)))
        .validate(h => if (h > 0) success else failure("Export height must be > 0")),
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

  @tailrec
  private final def readTx(in: CodedInputStream): ByteString | SignedTransaction =
    in.readTag() match {
      case 0 => throw new IllegalArgumentException("no tx found")
      case 42 => SignedTransaction(SignedTransaction.Transaction.EthereumTransaction(in.readBytes()))
      case 18 => scalapb.LiteParser.readMessage[SignedTransaction](in)
      case 10 => in.readBytes()
      case 24 =>
        in.readEnum()
        readTx(in)
      case 32 =>
        in.readInt64()
        readTx(in)
      case tag =>
        throw new IllegalArgumentException("unexpected field")
    }

  def mkTxsDataBytes(header: BlockHeader, transactions: Seq[Array[Byte]]): Array[Byte] = {
    val transactionsDataBytes = writeTransactionData(header.version, transactions)
    Bytes.concat(
      Ints.toByteArray(transactionsDataBytes.length),
      transactionsDataBytes
    )
  }

  def writeTransactionData(version: Byte, txsBytes: Seq[Array[Byte]]): Array[Byte] = {
    val txsBytesSize = txsBytes.map(_.length + Ints.BYTES).sum
    val txsBuf = ByteBuffer.allocate(txsBytesSize)
    txsBytes.foreach(tx => txsBuf.putInt(tx.length).put(tx))

    Bytes.concat(mkTxsCountBytes(version, txsBytes.size), txsBuf.array())
  }
}
