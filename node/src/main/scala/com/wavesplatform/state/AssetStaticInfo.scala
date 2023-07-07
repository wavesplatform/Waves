package com.wavesplatform.state

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr

case class AssetStaticInfo(id: ByteStr, source: TransactionId, issuer: PublicKey, decimals: Int, nft: Boolean)
