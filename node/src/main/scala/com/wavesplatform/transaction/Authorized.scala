package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
