package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.crypto.Merkle.BalanceProof
import play.api.libs.json._

case class ProvenBalance(address: Address, proofForMiningBalance: BalanceProof, proofForRegularBalance: BalanceProof, balanceInfo: BalanceInfo)

object ProvenBalance {
  implicit val writes: Writes[ProvenBalance] = Json.writes[ProvenBalance]
}
