package scorex.crypto

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

  def sign(privateKey: PrivateKey, publicKey: PublicKey, message: MessageToSign): Signature

  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature =
    Try(sign(account.privateKey, account.publicKey, message)).ensuring(_.isSuccess).get

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean
}

object SigningFunctionsImpl extends SigningFunctions {

  import Sha256._

  override val SignatureLength = 64
  override val KeyLength = Curve25519.KEY_SIZE

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val privateKey = new Array[Byte](KeyLength)
    val publicKey = new Array[Byte](KeyLength)
    Curve25519.keygen(publicKey, privateKey, seed)
    privateKey -> publicKey
  }

  override def sign(privateKey: PrivateKey, publicKey: PublicKey, message: MessageToSign): Signature = {
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

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
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