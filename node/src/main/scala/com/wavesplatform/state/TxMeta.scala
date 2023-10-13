package com.wavesplatform.state

case class TxMeta(height: Height, succeeded: Boolean, spentComplexity: Long)
object TxMeta {
  val empty = TxMeta(Height(0), false, 0)
}
