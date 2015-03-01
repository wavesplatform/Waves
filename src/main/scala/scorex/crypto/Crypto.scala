package scorex.crypto

import java.security.MessageDigest

import scorex.account.{Account, PrivateKeyAccount}

import scala.util.Try


object Crypto {

  val ADDRESS_VERSION: Byte = 58

  def createKeyPair(seed: Array[Byte]) = Curve25519Impl.createKeyPair(seed)

  def getAddress(publicKey: Array[Byte]) = {
    //SHA256 PUBLICKEY FOR PROTECTION THEN RIPEMD160 TO CREATE A SHORTER ADDRESS
    val publicKeyHash = new RIPEMD160().digest(sha256(publicKey))

    //CONVERT TO LIST
    val withoutChecksum = publicKeyHash :+ ADDRESS_VERSION //prepend ADDRESS_VERSION

    //GENERATE CHECKSUM
    val checkSum = doubleSha256(withoutChecksum)

    //ADD FIRST 4 BYTES OF CHECKSUM TO ADDRESS
    //BASE58 ENCODE ADDRESS
    Base58.encode(withoutChecksum ++ checkSum.take(4))
  }

  def isValidAddress(address: String) =
    Try {
      //BASE 58 DECODE
      val addressBytes = Base58.decode(address)

      //CHECK BYTES
      if (addressBytes.length != Account.ADDRESS_LENGTH) false
      //else if(addressBytes.head != ADDRESS_VERSION) false    todo:???
      else {
        val checkSum = addressBytes.takeRight(4)

        //GENERATE ADDRESS CHECKSUM
        val digest = doubleSha256(addressBytes.dropRight(4))
        val checkSumTwo = digest.take(4)

        //CHECK IF CHECKSUMS ARE THE SAME
        checkSum.sameElements(checkSumTwo)
      }
    }.getOrElse(false)

  def doubleSha256(input: Array[Byte]) = sha256(sha256(input))

  def sha256(input: Array[Byte]) = MessageDigest.getInstance("SHA-256").digest(input)

  //todo: return Try instead of unwrapping it
  def sign(account: PrivateKeyAccount, message: Array[Byte]) =
    Try(Curve25519Impl.sign(account.privateKey, account.publicKey, message))
      .getOrElse(Array.fill(64)(0: Byte))

  def verify(publicKey: Array[Byte], signature: Array[Byte], message: Array[Byte]) =
    Try(Curve25519Impl.verify(signature, message, publicKey)).getOrElse(false)
}