package scorex.crypto.hash

/**
  * Fast and secure hash function
  */
object FastCryptographicHash extends CryptographicHash32 {
  override def hash(in: Message): Digest32 = Blake2b256.hash(in)
}
