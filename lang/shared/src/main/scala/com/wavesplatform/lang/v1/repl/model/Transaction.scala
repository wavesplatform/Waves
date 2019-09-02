package com.wavesplatform.lang.v1.repl.model

trait Transaction extends Signable {
  def fee: Long
  def timestamp: Long
  def height: Int
  def `type`: Byte
  def version: Byte
}

object Transaction {
  val V1 = 1
  val V2 = 2

  //implicit val r: Reader[Transaction] = macroR
}


object InvokeScriptTransaction {
  //implicit val r: Reader[InvokeScriptTransaction] = macroR
}
