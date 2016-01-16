package scorex.crypto.hash

import scorex.crypto.hash.CryptographicHash._

/**
  * Interface for fast and secure Blake hash function
  */

object FastCryptographicHash extends CryptographicHash {

  override val DigestSize: Int = Blake256.DigestSize

  override def hash(in: Message): Digest = Blake256.hash(in)

}
