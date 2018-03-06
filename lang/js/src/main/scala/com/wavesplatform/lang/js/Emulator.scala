package com.wavesplatform.lang.js

import com.wavesplatform.lang.traits.Transaction

trait Emulator extends com.wavesplatform.lang.traits.Emulator {
  def height: Int                                           = Global.height
  def transaction: Transaction                              = Global.transaction
  def transactionById(id: Array[Byte]): Option[Transaction] = Global.transactionById(id)
}
