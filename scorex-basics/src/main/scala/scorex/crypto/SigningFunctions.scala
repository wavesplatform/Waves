package scorex.crypto

import java.lang.reflect.Constructor
import java.security.SecureRandom

import org.whispersystems.curve25519.OpportunisticCurve25519Provider
import scorex.account.PrivateKeyAccount
import scorex.utils.ScorexLogging

import scala.util.{Failure, Try}

trait SigningFunctions {

  import SigningFunctions._

  val SignatureLength: Int
  val KeyLength: Int

  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey)

  def createKeyPair: (PrivateKey, PublicKey) = {
    val seed = new Array[Byte](KeyLength)
    new SecureRandom().nextBytes(seed) // modifies seed
    createKeyPair(seed)
  }

  def sign(privateKey: PrivateKey, message: MessageToSign): Signature

  def sign(account: PrivateKeyAccount, message: MessageToSign): Signature =
    Try(sign(account.privateKey, message)).ensuring(_.isSuccess).get

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean
}

object SigningFunctions {
  type PrivateKey = Array[Byte]
  type PublicKey = Array[Byte]
  type Signature = Array[Byte]
  type MessageToSign = Array[Byte]
}

trait EllipticCurve extends SigningFunctions


trait Curve25519 extends EllipticCurve with ScorexLogging {

  import SigningFunctions._

  override val SignatureLength = 64
  override val KeyLength = 32

  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider]
      .getDeclaredConstructors
      .head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val hashedSeed = Sha256.hash(seed)
    val privateKey = provider.generatePrivateKey(hashedSeed)
    privateKey -> provider.generatePublicKey(privateKey)
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.length == KeyLength)
    provider.calculateSignature(provider.getRandom(64), privateKey, message)
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
    require(signature.length == SignatureLength)
    require(publicKey.length == KeyLength)
    provider.verifySignature(publicKey, message, signature)
  }.recoverWith { case e =>
    log.debug("Error while message signature verification", e)
    Failure(e)
  }.getOrElse(false)
}


object EllipticCurveImpl extends Curve25519