package com.wavesplatform.matcher
import scala.math.BigDecimal.RoundingMode.CEILING

class AssetPairDecimals(amountDecimals: Byte, priceDecimals: Byte) {
  def amount(a: Double): Long         = (BigDecimal(a) * Math.pow(10, amountDecimals)).toLong
  def price(p: Double): Long          = (BigDecimal(p) * Math.pow(10, 8 + priceDecimals - amountDecimals)).toLong
  def minAmountFor(price: Long): Long = (BigDecimal(Math.pow(10, amountDecimals)) / BigDecimal(price)).setScale(0, CEILING).toLong
}
