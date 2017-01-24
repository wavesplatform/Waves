package scorex.crypto

import org.whispersystems.curve25519.Curve25519
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.SigningFunctions.{MessageToSign, Signature}

/**
  * This implementation is being used from many places in the code. We consider easy switching from one
  * EC implementation from another as possible option, while switching to some other signature schemes
  * (e.g. hash-based signatures) will require a lot of code changes around the project(at least because of
  * big signature size).
  */
object EllipticCurveImpl {
  val crv = Curve25519.getInstance(Curve25519.JAVA)

  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature = sign(account.privateKey, message)

  def sign(privateKey: Array[Byte], message: MessageToSign): Signature = crv.calculateSignature(privateKey, message)

  def verify(pub: Array[Byte], message: Array[Byte], sig: Array[Byte]): Boolean = crv.verifySignature(pub, message, sig)

  def createPair = crv.generateKeyPair()
}
