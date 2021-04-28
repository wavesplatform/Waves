package com.wavesplatform.it

import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.settings.Constants

package object util {
  implicit class DoubleExt(val d: Double) extends AnyVal {
    def waves: Long = (BigDecimal(d) * Constants.UnitsInWave).toLong
  }

  implicit class AddressOrAliasExt(val a: AddressOrAlias) extends AnyVal {
    def toRide: Recipient =
      a match {
        case address: Address => Recipient.Address(ByteStr(address.bytes))
        case alias: Alias     => Recipient.Alias(alias.name)
        case _                => ???
      }
  }
}
