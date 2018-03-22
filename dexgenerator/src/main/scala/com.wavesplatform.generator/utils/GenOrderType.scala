package com.wavesplatform.generator.utils


object GenOrderType extends Enumeration {

  val ActiveBuy = Value(1)
  val ActiveSell = Value(2)
  val InvalidAmountOrder = Value(3)
  val OrderBuy = Value(4)
  val OrderSell = Value(5)
  val FakeOrder = Value(6)


}