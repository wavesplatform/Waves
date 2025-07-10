package com.wavesplatform

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.database.{KeyTag, RDB, RocksDBWriter}
import com.wavesplatform.utils.ScorexLogging
import org.rocksdb.{FlushOptions, ReadOptions, WriteBatch, WriteOptions}

import java.io.File
import java.util
import scala.util.Using

object Cleaner extends ScorexLogging {
  private def cleanupStaleHistory(
      rdb: RDB,
      writeOptions: WriteOptions,
      keyTag: KeyTag,
      blockchainHeight: Int,
      extractAddressId: Array[Byte] => Long,
      extractKey: Array[Byte] => Array[Byte]
  ): Unit =
    Using.Manager { use =>
      log.info(s"Removing stale values for $keyTag")
      val iterator = use(rdb.db.newIterator(use(new ReadOptions().setVerifyChecksums(false).setTotalOrderSeek(true))))
      iterator.seek(Shorts.toByteArray((keyTag.ordinal + 1).toShort))
      iterator.prev()
      type Entry = (addressId: Long, key: Array[Byte], height: Int)
      var prevNode: Entry = (0L, Array.emptyByteArray, blockchainHeight)
      var counter             = 0
      var writeBatch          = new WriteBatch()
      while (iterator.isValid && iterator.key().startsWith(keyTag.prefixBytes)) {
        if (iterator.isValid) {
          val height    = Ints.fromByteArray(iterator.key().takeRight(4))
          val addressId = extractAddressId(iterator.key())
          val key       = extractKey(iterator.key())
          if (prevNode.addressId == addressId && util.Arrays.equals(prevNode.key, key) && prevNode.height < blockchainHeight - 2000) {
            writeBatch.delete(iterator.key())
            counter += 1
            if (counter >= 1000000) {
              log.info("Deleting 1000000 entries")
              rdb.db.write(writeOptions, writeBatch)
              writeBatch.close()
              writeBatch = new WriteBatch()
              counter = 0
            }
          }
          prevNode = (addressId, key, height)

        } else {
          log.warn("Iterator is invalid")
        }
        iterator.prev()
      }
      rdb.db.write(writeOptions, writeBatch)
      writeBatch.close()
    }.get

  def main(args: Array[String]): Unit = {
    val settings = Application.loadApplicationConfig(args.headOption.map(new File(_)))

    log.info(s"Data directory: ${settings.dbSettings.directory}")

    val rdb    = RDB.open(settings.dbSettings)
    val reader = RocksDBWriter(rdb, settings.blockchainSettings, settings.dbSettings, settings.enableLightMode)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    log.info("Dropping snapshots column family")
    rdb.db.dropColumnFamily(rdb.txSnapshotHandle.handle)
    log.info("Dropping API column family")
    rdb.db.dropColumnFamily(rdb.apiHandle.handle)
    log.info("Dropping TX column family")
    rdb.db.dropColumnFamily(rdb.txHandle.handle)

    val writeOptions = new WriteOptions().setDisableWAL(true)
    log.info("Deleting changed keys")
    rdb.db.deleteRange(writeOptions, KeyTag.ChangedAddresses.prefixBytes, KeyTag.AddressIdOfAlias.prefixBytes)

    cleanupStaleHistory(rdb, writeOptions, KeyTag.DataHistory, blockchainHeight, bs => Longs.fromByteArray(bs.slice(2, 10)), bs => bs.drop(10).dropRight(4))
    cleanupStaleHistory(rdb, writeOptions, KeyTag.AssetBalanceHistory, blockchainHeight, bs => Longs.fromByteArray(bs.slice(34, 42)), bs => bs.slice(2, 34))
    cleanupStaleHistory(rdb, writeOptions, KeyTag.WavesBalanceHistory, blockchainHeight, bs => Longs.fromByteArray(bs.slice(2, 10)), _ => Array.emptyByteArray)

    log.info("Compacting database")
    rdb.db.compactRange()

    Using(new FlushOptions().setWaitForFlush(true)) { flushOptions =>
      log.info("Flushing data to disk")
      rdb.db.flush(flushOptions)
    }
    log.info("Closing database")
    rdb.close()
  }
}
