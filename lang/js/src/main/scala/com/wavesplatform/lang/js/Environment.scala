package com.wavesplatform.lang.js

import com.wavesplatform.lang.traits.Transaction

trait Environment extends com.wavesplatform.lang.traits.Environment {
  def height: Int                                           = Global.height
  def networkByte: Byte                                     = Global.networkByte
  def transaction: Transaction                              = Global.transaction
  def transactionById(id: Array[Byte]): Option[Transaction] = Global.transactionById(id)
}
