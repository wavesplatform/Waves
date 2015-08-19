package scorex.crypto

import java.security.MessageDigest

import scorex.account.PrivateKeyAccount

import scala.util.Try

/**
 * sign & verify functions defined in the same way as in Nxt 
 */

object Crypto {
  val SignatureLength = 64
  val KeyLength = Curve25519.KEY_SIZE

  def doubleSha256(input: Array[Byte]) = sha256(sha256(input))

  def sha256(input: Array[Byte]) = MessageDigest.getInstance("SHA-256").digest(input)


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

    val m = sha256(message)
    val x = sha256(m ++ privateKey)

    val Y = new Array[Byte](KeyLength)
    Curve25519.keygen(Y, null, x)
    val h = sha256(m ++ Y)

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

    val m = sha256(message)
    val h2 = sha256(m ++ Y)

    h.sameElements(h2)
  }.getOrElse(false)
}