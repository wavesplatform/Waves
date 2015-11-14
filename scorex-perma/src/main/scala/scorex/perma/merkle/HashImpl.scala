package scorex.perma.merkle

import java.security.MessageDigest

import scorex.perma.merkle.CryptographicHash.Digest

object HashImpl extends CryptographicHash {
  private val hasher = MessageDigest.getInstance("SHA-1")

  override def hash(byteSequence: Seq[Byte]): Digest =
    hasher.digest(byteSequence.toArray)
}
