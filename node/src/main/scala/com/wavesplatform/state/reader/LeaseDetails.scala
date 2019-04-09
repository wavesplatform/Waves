package com.wavesplatform.state.reader

import com.wavesplatform.account.{PublicKey, AddressOrAlias}

case class LeaseDetails(sender: PublicKey, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
