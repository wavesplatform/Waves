package scorex.crypto.hash



/**
  * The chain of two hash functions, Blake and Keccak
  */

object ScorexHashChain extends CryptographicHash {

  override val DigestSize: Int = 32

  override def hash(in: Message): Digest = scorex.utils.HashHelpers.applyHashes(in, Blake256, Keccak256)
}
