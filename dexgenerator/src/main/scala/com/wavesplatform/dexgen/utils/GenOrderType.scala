package com.wavesplatform.dexgen.utils

object GenOrderType extends Enumeration {

  val ActiveBuy     = Value(1)
  val ActiveSell    = Value(2)
  val InvalidAmount = Value(3)
  val Buy           = Value(4)
  val Sell          = Value(5)
  val FakeSell      = Value(6)
  val FakeBuy       = Value(7)
  val Cancel        = Value(8)

}
