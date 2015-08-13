package scorex.crypto

import java.security.MessageDigest
import scorex.account.{Account, PrivateKeyAccount}
import scala.util.Try

/**
 * sign & verify functions defined in the same way as in Nxt 
 */

object Crypto {

  val AddressVersion: Byte = 58
  val SignatureLength = 64
  val KeyLength = 32
  

  def getAddress(publicKey: Array[Byte]) = {
    val publicKeyHash = new RIPEMD160().digest(sha256(publicKey))
    val withoutChecksum = publicKeyHash :+ AddressVersion //prepend ADDRESS_VERSION
    val checkSum = doubleSha256(withoutChecksum)

    Base58.encode(withoutChecksum ++ checkSum.take(4))
  }

  def isValidAddress(address: String) =
    Base58.decode(address).map{addressBytes =>

      //CHECK BYTES
      if (addressBytes.length != Account.AddressLength)
        false
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

  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    Try(sign(account.privateKey, account.publicKey, message)).ensuring(_.isSuccess).get

  def sign(privateKey: Array[Byte], publicKey: Array[Byte], message: Array[Byte]): Array[Byte] = {
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

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val privateKey = new Array[Byte](KeyLength)
    val publicKey = new Array[Byte](KeyLength)
    Curve25519.keygen(publicKey, privateKey, seed)
    privateKey -> publicKey
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = Try {
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