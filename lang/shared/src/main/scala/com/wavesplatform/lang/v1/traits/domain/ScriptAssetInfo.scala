package com.wavesplatform.lang.v1.traits.domain
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address

case class ScriptAssetInfo(
    totalAmount: Long,
    decimals:    Int,
    issuer:      Address,
    issuerPk:    ByteStr,
    reissuable:  Boolean,
    scripted:    Boolean,
    sponsored:   Boolean
)
