package scorex.crypto.hash

import scorex.crypto._
import scorex.crypto.hash.CryptographicHash._


/**
  * The chain of two hash functions, Blake and Keccak
  */

object ScorexHashChain extends CryptographicHash {

  override val DigestSize: Int = 32

  override def hash(in: Message): Digest = applyHashes(in, Blake256, Keccak256)
}
