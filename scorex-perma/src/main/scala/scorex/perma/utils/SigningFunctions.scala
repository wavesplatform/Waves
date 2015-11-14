package scorex.perma.utils

import net.vrallev.java.ecc.{Ecc25519Helper, KeyHolder}

import scala.util.{Failure, Try}

//todo: copypasted from scorex, remove after merging

trait SigningFunctions {
  type PrivateKey = Array[Byte]
  type PublicKey = Array[Byte]
  type Signature = Array[Byte]
  type MessageToSign = Array[Byte]

  val SignatureLength: Int
  val KeyLength: Int

  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey)

  def sign(privateKey: PrivateKey, message: MessageToSign): Signature
  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean
}

object SigningFunctionsImpl extends SigningFunctions {

  override val SignatureLength = 64
  override val KeyLength = 32

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val privateKey = KeyHolder.createPrivateKey(seed)
    val kh = new KeyHolder(privateKey)

    kh.getPrivateKey -> kh.getPublicKeySignature
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.length == KeyLength)
    new Ecc25519Helper(privateKey).sign(message)
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
    require(signature.length == SignatureLength)
    require(publicKey.length == KeyLength)
    new Ecc25519Helper().isValidSignature(message, signature, publicKey)
  }.recoverWith { case e =>
    println("Error while message signature verification", e)
    Failure(e)
  }.getOrElse(false)
}