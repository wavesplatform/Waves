package scorex.crypto

import java.security.MessageDigest

import scorex.account.PrivateKeyAccount

import scala.util.Try


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


/**
 * sign & verify functions defined in the same way as in Nxt
 */

object Crypto {

  import HashFunctionsImpl._

  val SignatureLength = 64
  val KeyLength = Curve25519.KEY_SIZE

  type PrivateKey = Array[Byte]
  type PublicKey = Array[Byte]
  type Signature = Array[Byte]
  type MessageToSign = Array[Byte]

  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val privateKey = new Array[Byte](KeyLength)
    val publicKey = new Array[Byte](KeyLength)
    Curve25519.keygen(publicKey, privateKey, seed)
    privateKey -> publicKey
  }

  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature =
    Try(sign(account.privateKey, account.publicKey, message)).ensuring(_.isSuccess).get

  def sign(privateKey: PrivateKey, publicKey: PublicKey, message: MessageToSign): Signature = {
    require(privateKey.length == KeyLength)
    require(publicKey.length == KeyLength)

    val m = hash(message)
    val x = hash(m ++ privateKey)

    val Y = new Array[Byte](KeyLength)
    Curve25519.keygen(Y, null, x)
    val h = hash(m ++ Y)

    val v = new Array[Byte](KeyLength)
    Curve25519.sign(v, h, x, privateKey)
    v ++ h
  }

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
    require(signature.length == SignatureLength)
    require(publicKey.length == KeyLength)

    val v = new Array[Byte](KeyLength)
    val h = new Array[Byte](KeyLength)

    System.arraycopy(signature, 0, v, 0, KeyLength)
    System.arraycopy(signature, KeyLength, h, 0, KeyLength)

    val Y = new Array[Byte](KeyLength)
    Curve25519.verify(Y, v, h, publicKey)

    val m = hash(message)
    val h2 = hash(m ++ Y)

    h.sameElements(h2)
  }.getOrElse(false)
}