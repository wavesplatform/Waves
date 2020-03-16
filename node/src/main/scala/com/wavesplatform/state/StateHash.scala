package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.StateHash.SectionId

case class StateHash(totalHash: ByteStr, bySection: Map[SectionId.Value, ByteStr])

object StateHash {
  val empty = new StateHash(Array.fill(crypto.DigestSize)(0.toByte), Map.empty)

  object SectionId extends Enumeration {
    val Balance, DataChanges, Alias, SetScript, SetAssetScript, Lease, Sponsor = Value
  }
}
