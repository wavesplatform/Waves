package scorex.crypto

import scorex.account.PrivateKeyAccount
import scorex.crypto.signatures.Curve25519
import scorex.crypto.signatures.SigningFunctions.{MessageToSign, PrivateKey, PublicKey, Signature}

/**
  * This implementation is being used from many places in the code. We consider easy switching from one
  * EC implementation from another as possible option, while switching to some other signature schemes
  * (e.g. hash-based signatures) will require a lot of code changes around the project(at least because of
  * big signature size).
  */
object EllipticCurveImpl {
  def sign(privateKey: PrivateKey, message: MessageToSign): Signature = Curve25519.sign(privateKey, message)

  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature = sign(account.privateKey, message)

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Curve25519.verify(signature, message, publicKey)

  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = Curve25519.createKeyPair(seed)

  val SignatureLength: Int = Curve25519.SignatureLength
}
