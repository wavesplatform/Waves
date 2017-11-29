package com.wavesplatform

import java.io.File

import org.iq80.leveldb.{DB, Options}

package object db {

  def openDB(path: String): DB = {
    val options = new Options()
    options.createIfMissing(true)

    val file = new File(path)
    file.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

}
