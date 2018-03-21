package com.wavesplatform

import java.io.File

import org.iq80.leveldb.{DB, Options}
import scorex.utils.ScorexLogging

package object db extends ScorexLogging {

  def openDB(path: String, cacheSizeBytes: Long, recreate: Boolean = false): DB = {
    log.debug(s"Open DB at $path")
    val file = new File(path)
    val options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true)
      .cacheSize(cacheSizeBytes)

    if (recreate) {
      LevelDBFactory.factory.destroy(file, options)
    }

    file.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

}
