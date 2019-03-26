package com.wavesplatform.transaction
import com.wavesplatform.account.AccountPublicKey

trait Authorized {
  val sender: AccountPublicKey
}
