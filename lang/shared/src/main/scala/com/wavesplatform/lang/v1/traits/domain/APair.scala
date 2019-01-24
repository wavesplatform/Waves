package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr

case class APair(amountAsset: Option[ByteStr], priceAsset: Option[ByteStr])
