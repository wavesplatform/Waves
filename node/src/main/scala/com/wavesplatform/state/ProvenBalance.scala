package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.crypto.Merkle.BalanceProof
import play.api.libs.json._

case class ProvenBalance(address: Address, proofForMiningBalance: BalanceProof, proofForRegularBalance: BalanceProof, balanceInfo: BalanceInfo)

object ProvenBalance {
  implicit val format: Writes[ProvenBalance] = Writes { provenBalance =>
    import provenBalance._

    Json
      .obj(
        "address"                -> JsString(address.stringRepr),
        "proofForMiningBalance"  -> Json.toJson(proofForMiningBalance),
        "proofForRegularBalance" -> Json.toJson(proofForRegularBalance),
        "balanceInfo"            -> Json.toJson(balanceInfo)
      )
  }
}
