package com.wavesplatform.lang.v1.evaluator.ctx.impl

object GlobalValNames {
  val Sell: String      = "Sell"
  val Buy: String       = "Buy"
  val Height: String    = "height"
  val LastBlock: String = "lastBlock"
  val Tx: String        = "tx"
  val This: String      = "this"
  val Nil: String       = "nil"
  val Unit: String      = "unit"

  val All: Set[String] =
    Set(Buy, Sell, Height, LastBlock, Nil, This, Tx, Unit) ++
      Rounding.fromV5.map(_.definition._1) ++
      CryptoContext.rsaTypeNames.map(_.toUpperCase)
}
