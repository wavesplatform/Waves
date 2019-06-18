package com.wavesplatform.state.reader

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.state.Height

case class LeaseDetails(sender: PublicKey, recipient: AddressOrAlias, height: Height, amount: Long, isActive: Boolean)
