package com.wavesplatform.account

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._

final case class OptionalAddress(bytes: ByteStr) {
  def isEmpty = bytes.isEmpty

  lazy val toAddressOrAlias: AddressOrAlias = AddressOrAlias.fromBytes(bytes, 0).explicitGet()._1
  def toAddress: Address                    = Address.fromBytes(bytes).explicitGet()
  def toAlias: Alias                        = Alias.fromBytes(bytes).explicitGet()
}

object OptionalAddress {
  val empty = OptionalAddress(ByteStr.empty)

  implicit def apply(addressOrAlias: AddressOrAlias): OptionalAddress = OptionalAddress(addressOrAlias.bytes)
  implicit def apply(address: Address): OptionalAddress               = OptionalAddress(address.bytes)
  implicit def apply(alias: Alias): OptionalAddress                   = OptionalAddress(alias.bytes)
}
