package com.wavesplatform.state.reader

import com.wavesplatform.account.{AccountPublicKey, AddressOrAlias}

case class LeaseDetails(sender: AccountPublicKey, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
