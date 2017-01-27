package scorex.waves.crypto

import scorex.crypto.applyHashes
import scorex.crypto.hash.SecureCryptographicHash.{Message, Digest}
import scorex.crypto.hash.{Blake2b256, CryptographicHash, Keccak256}

object HashChain extends CryptographicHash {

  override val DigestSize: Int = 32

  override def hash(in: Message): Digest = applyHashes(in, Blake2b256, Keccak256)
}
