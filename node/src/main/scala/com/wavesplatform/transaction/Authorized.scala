package com.wavesplatform.transaction
import com.wavesplatform.account.PublicKey

trait Authorized {
  def sender: PublicKey
}
