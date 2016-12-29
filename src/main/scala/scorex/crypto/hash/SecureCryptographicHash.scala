package scorex.crypto.hash

import scorex.crypto.hash.CryptographicHash._


/**
 * Hash function for cases, where security is more important, then speed
 */
object SecureCryptographicHash extends CryptographicHash {

  private val hf: CryptographicHash = scorex.waves.crypto.HashChain

  override val DigestSize: Int = hf.DigestSize

  override def hash(in: Message): Digest = hf.hash(in)
}
