package scorex.account

import scorex.crypto.Crypto

class PublicKeyAccount(val publicKey: Array[Byte]) extends Account(Crypto.getAddress(publicKey))