package com.wavesplatform.db

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Ints
import org.iq80.leveldb.WriteBatch

trait PropertiesStorage {
  this: Storage =>

  private val PropertiesPrefix: Array[Byte] = "prop".getBytes(StandardCharsets.UTF_8)

  def putIntProperty(property: String, value: Int, batch: Option[WriteBatch]): Unit =
    put(makeKey(PropertiesPrefix, property), Ints.toByteArray(value), batch)

  def getIntProperty(property: String): Option[Int] = get(makeKey(PropertiesPrefix, property)).map(Ints.fromByteArray)

  def getProperty(property: String): Option[Array[Byte]] = get(makeKey(PropertiesPrefix, property))

  def putProperty(property: String, value: Array[Byte], batch: Option[WriteBatch]): Unit =
    put(makeKey(PropertiesPrefix, property), value, batch)
}
