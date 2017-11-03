package com.wavesplatform.db

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Ints

trait PropertiesStorage {
  this: Storage =>

  private val PropertiesPrefix: Array[Byte] = "prop".getBytes(StandardCharsets.UTF_8)

  def putInt(property: String, value: Int): Unit = put(makeKey(PropertiesPrefix, property), Ints.toByteArray(value))

  def getInt(property: String): Option[Int] = get(makeKey(PropertiesPrefix, property)).map(Ints.fromByteArray)

  def get(property: String): Option[Array[Byte]] = get(makeKey(PropertiesPrefix, property))

  def put(property: String, value: Array[Byte]): Unit = put(makeKey(PropertiesPrefix, property), value)
}

