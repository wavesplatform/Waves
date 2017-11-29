package com.wavesplatform

import java.io.File

import org.iq80.leveldb.{DB, Options}

package object db {

  def openDB(path: String, recreate: Boolean = false): DB = {
    val file = new File(path)
    val options = new Options()
    options.createIfMissing(true)

    if (recreate) {
      LevelDBFactory.factory.destroy(file, options)
    }

    file.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

}
