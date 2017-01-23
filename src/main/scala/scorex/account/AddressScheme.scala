package scorex.account

abstract class AddressScheme {
  val chainId: Byte
}

object AddressScheme {
  @volatile var current : AddressScheme = DefaultAddressScheme
}

object DefaultAddressScheme extends AddressScheme {
  val chainId: Byte = 'T'.toByte
}
