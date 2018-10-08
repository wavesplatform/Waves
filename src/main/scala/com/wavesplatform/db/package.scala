package com.wavesplatform

import java.io.File
import java.util

import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.{DB, DBIterator, Options}

package object db extends ScorexLogging {

  def openDB(path: String, recreate: Boolean = false): DB = {
    log.debug(s"Open DB at $path")
    val file = new File(path)
    val options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true)

    if (recreate) {
      LevelDBFactory.factory.destroy(file, options)
    }

    file.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

  def prefixIterator[T](iterator: DBIterator, prefix: Array[Byte])(deserialize: util.Map.Entry[Array[Byte], Array[Byte]] => T): Iterator[T] =
    new Iterator[T] {
      override def hasNext: Boolean = iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)
      override def next(): T        = deserialize(iterator.next())
    }

}
