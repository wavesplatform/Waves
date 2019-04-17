package com.wavesplatform.lang.v1.traits.domain
import com.wavesplatform.common.state.ByteStr

case class ScriptAssetInfo(totalAmount: Long,
                     decimals: Int,
                     issuer: ByteStr,
                     reissuable: Boolean,
                     scripted: Boolean,
                     sponsored: Boolean)
