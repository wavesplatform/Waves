package scorex.account

import scorex.crypto.Crypto

class PrivateKeyAccount(val seed:Array[Byte], val privateKey:Array[Byte], override val publicKey:Array[Byte])
	extends PublicKeyAccount(publicKey){

	def this(seed:Array[Byte], keyPair:(Array[Byte], Array[Byte])) = this(seed, keyPair._1, keyPair._2)
	def this(seed:Array[Byte]) = this(seed, Crypto.createKeyPair(seed))
}