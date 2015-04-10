package scorex.account

import scorex.crypto.Crypto
import scorex.database.PrunableBlockchainStorage

case class PrivateKeyAccount(seed: Array[Byte], privateKey: Array[Byte], override val publicKey: Array[Byte])
  extends PublicKeyAccount(publicKey) {

  override val address = Crypto.getAddress(publicKey)

  def this(seed: Array[Byte], keyPair: (Array[Byte], Array[Byte])) = this(seed, keyPair._1, keyPair._2)

  def this(seed: Array[Byte]) = this(seed, Crypto.createKeyPair(seed))

  def generatingBalance: BigDecimal = PrunableBlockchainStorage.generationBalance(address)
}