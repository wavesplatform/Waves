package com.wavesplatform.transaction
import com.wavesplatform.account.PublicKey

trait Authorized {
  val sender: PublicKey
}
