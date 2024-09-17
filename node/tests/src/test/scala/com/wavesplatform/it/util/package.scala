package com.wavesplatform.it

import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient

package object util {
  implicit class AddressOrAliasExt(val a: AddressOrAlias) extends AnyVal {
    def toRide: Recipient =
      a match {
        case address: Address => Recipient.Address(ByteStr(address.bytes))
        case alias: Alias     => Recipient.Alias(alias.name)
        case _                => ???
      }
  }
}
