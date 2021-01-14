package com.wavesplatform.state.reader

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr

case class LeaseDetails(sender: PublicKey, recipient: AddressOrAlias, txId: ByteStr, amount: Long, isActive: Boolean)
