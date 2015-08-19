package scorex.crypto

import java.security.MessageDigest

trait HashFunctions {
  def doubleHash(input: Array[Byte]): Array[Byte]

  def hash(input: Array[Byte]): Array[Byte]
}

/**
 * Hashing functions implementation with sha256 impl from Java SDK
 */
object HashFunctionsImpl extends HashFunctions {
  override def doubleHash(input: Array[Byte]) = hash(hash(input))

  override def hash(input: Array[Byte]) = MessageDigest.getInstance("SHA-256").digest(input)
}