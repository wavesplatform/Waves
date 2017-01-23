package scorex.crypto

import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.crypto.singing.Curve25519
import scorex.crypto.singing.SigningFunctions.{MessageToSign, PublicKey, Signature}

/**
  * This implementation is being used from many places in the code. We consider easy switching from one
  * EC implementation from another as possible option, while switching to some other signature schemes
  * (e.g. hash-based signatures) will require a lot of code changes around the project(at least because of
  * big signature size).
  */
object EllipticCurveImpl extends Curve25519 {
  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature = sign(account.privateKey, message)
}
