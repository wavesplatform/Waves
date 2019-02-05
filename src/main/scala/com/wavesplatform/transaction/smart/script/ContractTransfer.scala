package com.wavesplatform.transaction.smart.script
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.transaction.AssetId

case class ContractTransfer(assetId: Option[AssetId],
                            sender: Recipient.Address,
                            recipient: Recipient.Address,
                            amount: Long,
                            timestamp: Long,
                            id: ByteStr)
