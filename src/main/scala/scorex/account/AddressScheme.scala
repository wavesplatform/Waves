package scorex.account

abstract class AddressScheme {
  val chainId: Byte
}

object AddressScheme {
  var current : AddressScheme = DefaultAddressScheme
}

object DefaultAddressScheme extends AddressScheme {
  val chainId: Byte = 'T'.toByte
}
