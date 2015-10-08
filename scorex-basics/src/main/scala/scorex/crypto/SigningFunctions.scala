package scorex.crypto

import org.whispersystems.curve25519.Curve25519
import scorex.account.PrivateKeyAccount

import scala.util.Try

trait SigningFunctions {
  type PrivateKey = Array[Byte]
  type PublicKey = Array[Byte]
  type Signature = Array[Byte]
  type MessageToSign = Array[Byte]

  val SignatureLength: Int
  val KeyLength: Int

  //is it a real part of the interface? or not signing schemes have this kind of functionality?
  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey)

  def sign(privateKey: PrivateKey, message: MessageToSign): Signature

  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature =
    Try(sign(account.privateKey, message)).ensuring(_.isSuccess).get

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean
}

object SigningFunctionsImpl extends SigningFunctions {

  override val SignatureLength = 64
  override val KeyLength = 32

  private val cipher = Curve25519.getInstance(Curve25519.BEST)

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val keyPair = cipher.generateKeyPair()
    keyPair.getPrivateKey -> keyPair.getPublicKey
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.length == KeyLength)
    cipher.calculateSignature(privateKey, message)
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
    require(signature.length == SignatureLength)
    require(publicKey.length == KeyLength)

    cipher.verifySignature(publicKey, message, signature)
  }.getOrElse(false)      //todo: recover with log output
}