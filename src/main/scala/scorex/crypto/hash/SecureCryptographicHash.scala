package scorex.crypto.hash

import scorex.waves.crypto.HashChain


/**
 * Hash function for cases, where security is more important, then speed
 */
object SecureCryptographicHash extends CryptographicHash32 {
  override def hash(in: Message): Digest32 = HashChain.hash(in)
}
