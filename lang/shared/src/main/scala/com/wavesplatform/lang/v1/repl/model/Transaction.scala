package com.wavesplatform.lang.v1.repl.model

object Transaction {
  val V1 = 1
  val V2 = 2
}

trait Transaction extends Signable {
  def fee: Long
  def timestamp: Long
  def height: Int
  def `type`: Byte
  def version: Byte
}
