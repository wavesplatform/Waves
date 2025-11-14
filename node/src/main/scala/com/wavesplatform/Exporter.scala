package com.wavesplatform

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.{Bytes, Ints}
import com.google.protobuf.{ByteString, CodedInputStream}
import com.wavesplatform.block.serialization.{BlockSerializer, mkTxsCountBytes}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.database.protobuf.BlockMeta
import com.wavesplatform.database.{Caches, KeyTag, Keys, RDB, readBlockMeta}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.protobuf.block.PBBlocks.protobuf
import com.wavesplatform.protobuf.block.{PBBlock, PBBlocks}
import com.wavesplatform.protobuf.transaction.{PBTransactions, SignedTransaction}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.state.{Height, TxNum}
import com.wavesplatform.transaction.TransactionParsers
import com.wavesplatform.utils.*
import io.netty.buffer.{ByteBuf, PooledByteBufAllocator}
import kamon.Kamon
import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB}
import scopt.OParser

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
            Using.resources(createBufferedOutputStream(blocksOutput, 100), snapshotsOutput.map(createBufferedOutputStream(_, 100))) {
              case (blocksStream, _) =>
                var exportedBlocksBytes    = 0L
                var exportedSnapshotsBytes = 0L
                val start                  = System.currentTimeMillis()

                val txIterator = new DataIterator[ByteString | SignedTransaction](
                  rdb.db,
                  rdb.txHandle.handle,
                  KeyTag.NthTransactionInfoAtHeight.prefixBytes,
                  _.slice(2, 6),
                  _ => bytes => readTx(CodedInputStream.newInstance(bytes))
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

                var cumBlockSize  = 0
                val headers       = ArrayBuffer[BlockMeta]()
                val heightsAndNum = ArrayBuffer[(Height, TxNum)]()

                while (blockMetaIterator.hasNext) {
                  val thisBlockMeta = blockMetaIterator.next()._2
                  headers += thisBlockMeta
                  heightsAndNum += ((Height(thisBlockMeta.height), TxNum(thisBlockMeta.transactionCount.toShort)))
                  cumBlockSize += thisBlockMeta.size

                  if (cumBlockSize >= 50_000_000) {
                    val txs = readTransactionBatch(heightsAndNum.toSeq, rdb)

                    txs.zip(headers).foreach { case (txs, metaForTxs) =>
                      val signedHeader = Caches.toSignedHeader(metaForTxs)
                      val blockBytes =
                        if (signedHeader.header.version >= Block.ProtoBlockVersion)
                          PBUtils.encodeDeterministic(
                            new PBBlock(
                              Some(protobuf(signedHeader.header)),
                              ByteString.copyFrom(signedHeader.signature.arr),
                              txs.map {
                                case s: SignedTransaction => s
                                case bs: ByteString       => PBTransactions.protobuf(TransactionParsers.parseBytes(bs.toByteArray).get)
                              }.toSeq
                            )
                          )
                        else {
                          Bytes.concat(
                            BlockSerializer.mkPrefixBytes(signedHeader.header),
                            mkTxsDataBytes(signedHeader.header, txs.map(_.asInstanceOf[ByteString].toByteArray)),
                            BlockSerializer.mkSuffixBytes(signedHeader.header, signedHeader.signature)
                          )
                        }
                      blocksStream.write(Ints.toByteArray(blockBytes.length))
                      blocksStream.write(blockBytes)
                      exportedBlocksBytes += blockBytes.length
                    }

                    cumBlockSize = 0
                    headers.clear()
                    heightsAndNum.clear()
                    if ((counter + txs.size) / 1_000_000 - (counter / 1_000_000) > 0) {
                      log.info(f"Exported ${counter + txs.size}%,d transactions, written $exportedBlocksBytes%,d bytes")
                    }
                    counter += txs.size
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

    private def closeResources(): Unit = {
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
      case 0  => throw new IllegalArgumentException("no tx found")
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
        throw new IllegalArgumentException(s"unexpected field $tag")
    }

  def mkTxsDataBytes(header: BlockHeader, transactions: Iterable[Array[Byte]]): Array[Byte] = {
    val transactionsDataBytes = writeTransactionData(header.version, transactions)
    Bytes.concat(
      Ints.toByteArray(transactionsDataBytes.length),
      transactionsDataBytes
    )
  }

  def writeTransactionData(version: Byte, txsBytes: Iterable[Array[Byte]]): Array[Byte] = {
    val txsBytesSize = txsBytes.map(_.length + Ints.BYTES).sum
    val txsBuf       = ByteBuffer.allocate(txsBytesSize)
    txsBytes.foreach(tx => txsBuf.putInt(tx.length).put(tx))

    Bytes.concat(mkTxsCountBytes(version, txsBytes.size), txsBuf.array())
  }

  def readTransactionBatch(heightsAndCount: Seq[(Height, TxNum)], rdb: RDB): Iterable[Iterable[ByteString | SignedTransaction]] = {
    val (_, idx, cfHandles, allKeys, valueBufferB) =
      heightsAndCount.foldLeft((0, mutable.Buffer.empty[(Int, Int)], Vector.newBuilder[ColumnFamilyHandle], Vector.newBuilder[ByteBuffer], Vector.newBuilder[ByteBuf])) {
        case ((prev, indices, cfhs, keys, vbufs), (height, txNum)) =>
          keys ++= Vector.tabulate(txNum)(i => ByteBuffer.wrap(Keys.transactionAt(height, TxNum(i.toShort), rdb.txHandle).keyBytes))
          cfhs ++= Iterator.fill(txNum)(rdb.txHandle.handle)
          vbufs ++= Iterator.fill(txNum)(PooledByteBufAllocator.DEFAULT.buffer(5*1024))
          indices += (prev -> (prev + txNum))
          (prev + txNum, indices, cfhs, keys, vbufs)
      }

    log.info(s"multiGet from ${heightsAndCount.head._1} to ${heightsAndCount.last._1}, total of ${allKeys.result().length} txs")
    
    val valueBuffers = valueBufferB.result()

    val allTransactions = rdb.db
      .multiGetByteBuffers(new ReadOptions(false, false), cfHandles.result().asJava, allKeys.result().asJava, valueBuffers.map(_.nioBuffer()).asJava)
      .asScala
      .zip(valueBuffers)
      .map()

    idx.view.map { case (from, until) =>
      if (from == until) Seq.empty[ByteString | SignedTransaction]
      else allTransactions.view.slice(from, until)
    }
  }
}
