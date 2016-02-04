package scorex.account


class PublicKeyAccount(val publicKey: Array[Byte]) extends Account(Account.fromPublicKey(publicKey))
