package com.wavesplatform.state

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxPositiveAmount
import play.api.libs.json.*

case class LeaseStaticInfo(
    sender: PublicKey,
    recipientAddress: Address,
    amount: TxPositiveAmount,
    sourceId: ByteStr,
    height: Int
)

object LeaseStaticInfo {
  implicit val amountWrites: Writes[TxPositiveAmount] = com.wavesplatform.transaction.posAmountWrites
  implicit val byteStrFormat: Format[ByteStr]         = com.wavesplatform.utils.byteStrFormat
  implicit val writes: OWrites[LeaseStaticInfo]       = Json.writes
}
