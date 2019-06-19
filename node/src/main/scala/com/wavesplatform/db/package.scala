package com.wavesplatform

import java.io.File

import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.{CompressionType, DB, Options}

package object db extends ScorexLogging {

  def openDB(path: String, recreate: Boolean = false): DB = {
    log.debug(s"Open DB at $path")
    val file = new File(path)
    val options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true)
      .compressionType(CompressionType.NONE)

    if (recreate) {
      LevelDBFactory.factory.destroy(file, options)
    }

    file.getAbsoluteFile.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

}
