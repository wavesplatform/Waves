package scorex.crypto

import java.security.MessageDigest

import scorex.account.{Account, PrivateKeyAccount}

import scala.util.Try

/*
sign & verify functions defined in the same way as in Nxt
 */

object Crypto {

  val ADDRESS_VERSION: Byte = 58

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

  //todo: return Try instead of unwrapping it
  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    Try(sign(account.privateKey, account.publicKey, message))
      .getOrElse(Array.fill(64)(0: Byte))

  def sign(privateKey: Array[Byte], publicKey: Array[Byte], message: Array[Byte]): Array[Byte] = {
    require(privateKey.length == 32)
    require(publicKey.length == 32)

    val m = sha256(message)
    val x = sha256(m ++ privateKey)

    val Y = new Array[Byte](32)
    Curve25519.keygen(Y, null, x)
    val h = sha256(m ++ Y)

    val v = new Array[Byte](32)
    Curve25519.sign(v, h, x, privateKey)
    v ++ h
  }

  def sha256(input: Array[Byte]) = MessageDigest.getInstance("SHA-256").digest(input)

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val privateKey = new Array[Byte](32)
    val publicKey = new Array[Byte](32)
    Curve25519.keygen(publicKey, privateKey, seed)
    privateKey -> publicKey
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = Try {
    require(signature.length == 64)
    require(publicKey.length == 32)

    val v = new Array[Byte](32)
    val h = new Array[Byte](32)

    System.arraycopy(signature, 0, v, 0, 32)
    System.arraycopy(signature, 32, h, 0, 32)

    val Y = new Array[Byte](32)
    Curve25519.verify(Y, v, h, publicKey)

    val m = sha256(message)
    val h2 = sha256(m ++ Y)

    h.sameElements(h2)
  }.getOrElse(false)
}