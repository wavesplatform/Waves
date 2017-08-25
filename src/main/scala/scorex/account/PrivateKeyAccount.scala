package scorex.account

import java.util.concurrent.ThreadLocalRandom

import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser

sealed trait PrivateKeyAccount extends PublicKeyAccount {
  def seed: Array[Byte]

  def privateKey: Array[Byte]
}

object PrivateKeyAccount {

  private case class PrivateKeyAccountImpl(seed: Array[Byte], privateKey: Array[Byte], publicKey: Array[Byte]) extends PrivateKeyAccount

  def apply(seed: Array[Byte]): PrivateKeyAccount = {
    val pair = EllipticCurveImpl.createKeyPair(seed)
    PrivateKeyAccountImpl(seed, pair._1, pair._2)
  }

  def random: PrivateKeyAccount = {
    val seed = Array.ofDim[Byte](TransactionParser.KeyLength)
    ThreadLocalRandom.current().nextBytes(seed)
    PrivateKeyAccount(seed)
  }
}
