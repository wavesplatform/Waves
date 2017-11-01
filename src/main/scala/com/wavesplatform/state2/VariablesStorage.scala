package com.wavesplatform.state2

import java.io.File

import com.google.common.primitives.Ints
import com.wavesplatform.utils.createStore
import org.iq80.leveldb.DB

import scala.util.Try

abstract class VariablesStorage() extends AutoCloseable {
  private val variables: DB = createStore(new File("variables.dat"))

  protected def putInt(key: String, value: Int): Int = {
    variables.put(key.getBytes, Ints.toByteArray(value))
    value
  }

  protected def getInt(key: String): Option[Int] = Try(Ints.fromByteArray(variables.get(key.getBytes))).toOption

  override def close(): Unit = variables.close()
}
