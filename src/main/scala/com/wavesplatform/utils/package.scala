package com.wavesplatform

package object utils {
  import scala.math._

  def base58Length(byteArrayLength: Int): Int = ceil(log(256) / log(58) * byteArrayLength).toInt
}
