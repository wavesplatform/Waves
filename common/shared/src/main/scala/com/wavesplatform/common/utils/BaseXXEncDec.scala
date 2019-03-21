package com.wavesplatform.common.utils

import scala.util.Try

trait BaseXXEncDec {
  def defaultDecodeLimit: Int

  def encode(array: Array[Byte]): String
  def decode(str: String): Array[Byte]

  def tryDecode(str: String): Try[Array[Byte]] = Try {
    this.decode(str)
  }

  def tryDecodeWithLimit(str: String, limit: Int = defaultDecodeLimit): Try[Array[Byte]] =
    Try {
      require(str.length <= limit, s"base58Decode input exceeds $limit")
      this.tryDecode(str)
    }.flatten
}
