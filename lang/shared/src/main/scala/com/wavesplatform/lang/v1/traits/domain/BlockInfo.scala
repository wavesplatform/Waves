package com.wavesplatform.lang.v1.traits.domain
import com.wavesplatform.common.state.ByteStr

case class BlockInfo(timestamp: Long,
                     height: Int,
                     generationSignature: ByteStr)
