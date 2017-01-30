package scorex.crypto.hash

/**
 * Fast and secure hash function
 */
object FastCryptographicHash extends CryptographicHash {

  private val hf: CryptographicHash = scorex.crypto.hash.Blake2b256

  override val DigestSize: Int = hf.DigestSize

  override def hash(in: Message): Digest = hf.hash(in)

}
