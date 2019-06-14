package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs}

//noinspection ScalaStyle,TypeAnnotation
object KeyHelpers {

  import KeyDsl.Implicits._

  def historyKey(name: String, prefix: Short, b: Array[Byte]) = Key(name, (prefix, b), readIntSeq, writeIntSeq)

  def intKey(name: String, prefix: Short, default: Int = 0): Key[Int] =
    Key(name, prefix, Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def longKey(name: String, prefix: Short, default: Long = 0): Key[Long] =
    Key(name, prefix, Option(_).fold(default)(Longs.fromByteArray), Longs.toByteArray)

  def bytesSeqNr(name: String, prefix: Short, b: Array[Byte], default: Int = 0): Key[Int] =
    Key(name, (prefix, b), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}
