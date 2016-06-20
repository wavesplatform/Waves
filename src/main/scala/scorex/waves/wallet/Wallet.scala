package scorex.waves.wallet

import com.google.common.primitives.{Bytes, Ints}
import scorex.account.PrivateKeyAccount
import scorex.crypto.hash.SecureCryptographicHash

/**
  * Utility functions for address generation.
  * TODO: Should be moved to Scorex
  */
object Wallet {

  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    new PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))

}
