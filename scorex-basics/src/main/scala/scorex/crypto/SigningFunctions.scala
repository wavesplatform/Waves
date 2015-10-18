package scorex.crypto

import net.vrallev.java.ecc.{Ecc25519Helper, KeyHolder}
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

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    require(seed.length == 32)

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
  }.getOrElse(false)      //todo: recover with log output
}