package com.wavesplatform.state.reader

import com.wavesplatform.account.AddressOrAlias

case class LeaseDetails(sender: PublicKeyAccount, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
