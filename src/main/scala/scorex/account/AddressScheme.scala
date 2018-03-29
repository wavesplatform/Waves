package scorex.account

abstract class AddressScheme {
  val chainId: Byte
  override def toString: String = s"AddressScheme($chainId)"
}
