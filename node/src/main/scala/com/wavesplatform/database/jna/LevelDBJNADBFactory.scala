package com.wavesplatform.database.jna

import java.io.File

import com.protonail.leveldb.jna.{LevelDB, LevelDBCompressionType, LevelDBOptions}
import org.iq80.leveldb.{CompressionType, DB, DBFactory, Options}

class LevelDBJNADBFactory extends DBFactory {
  private[this] def openJnaDatabase(path: File, options: Options): LevelDB = {
    val opts = LevelDBJNADBFactory.toJNAOptions(options)
    try new LevelDB(path.toString, opts)
    finally opts.close()
  }

  override def open(path: File, options: Options): DB = {
    val db = openJnaDatabase(path, options)
    new LevelDBJNADB(db)
  }

  override def destroy(path: File, options: Options): Unit = {
    val options1 = LevelDBJNADBFactory.toJNAOptions(options)
    try LevelDB.destroy(path.toString, options1)
    finally options1.close()
  }

  override def repair(path: File, options: Options): Unit = {
    val options1 = LevelDBJNADBFactory.toJNAOptions(options)
    try LevelDB.repair(path.toString, options1)
    finally options1.close()
  }
}

private object LevelDBJNADBFactory {
  def toJNAOptions(o1: Options): LevelDBOptions = {
    val opts = new LevelDBOptions
    opts.setBlockRestartInterval(o1.blockRestartInterval())
    opts.setBlockSize(o1.blockSize())
    opts.setCompressionType(o1.compressionType() match {
      case CompressionType.NONE   => LevelDBCompressionType.NoCompression
      case CompressionType.SNAPPY => LevelDBCompressionType.SnappyCompression
    })
    opts.setCreateIfMissing(o1.createIfMissing())
    opts.setErrorIfExists(o1.errorIfExists())
    opts.setMaxOpenFiles(o1.maxOpenFiles())
    opts.setParanoidChecks(o1.paranoidChecks())
    opts.setWriteBufferSize(o1.writeBufferSize())
    opts
  }
}
