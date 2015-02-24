package scorex.account

import database.PrunableBlockchainStorage
import scorex.crypto.Crypto

case class PrivateKeyAccount(seed:Array[Byte], privateKey:Array[Byte], override val publicKey:Array[Byte])
	extends PublicKeyAccount(publicKey){

	def this(seed:Array[Byte], keyPair:(Array[Byte], Array[Byte])) = this(seed, keyPair._1, keyPair._2)
	def this(seed:Array[Byte]) = this(seed, Crypto.createKeyPair(seed))

	def generatingBalance:BigDecimal = PrunableBlockchainStorage.generationBalance(address)
}