package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.Account

trait Signable {
  def senderPublicKey: Account
}
