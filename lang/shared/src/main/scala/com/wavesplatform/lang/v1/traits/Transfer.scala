package com.wavesplatform.lang.v1.traits

trait Transfer {
  def address: AddressOrAlias
  def amount: Long
}
