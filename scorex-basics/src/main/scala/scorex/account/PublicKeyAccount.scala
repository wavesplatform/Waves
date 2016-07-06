package scorex.account

@SerialVersionUID(-5511437096393374460L)
class PublicKeyAccount(val publicKey: Array[Byte]) extends Account(Account.fromPublicKey(publicKey))
