package com.wavesplatform.common.utils
import scala.util.control.NonFatal

object Base58 extends BaseXXEncDec {
  private[this] val useSlowBase58: Boolean = sys.props.get("waves.use-slow-base58").exists(s => s.toLowerCase == "true" || s == "1")
  override val defaultDecodeLimit: Int     = 192

  override def encode(array: Array[Byte]): String = {
    if (useSlowBase58) {
      StdBase58.encode(array)
    } else {
      try {
        FastBase58.encode(array)
      } catch {
        case NonFatal(_) =>
          StdBase58.encode(array)
      }
    }
  }

  override def decode(str: String): Array[Byte] = {
    if (useSlowBase58) {
      StdBase58.decode(str)
    } else {
      try {
        FastBase58.decode(str)
      } catch {
        case NonFatal(_) =>
          StdBase58.decode(str)
      }
    }
  }
}
