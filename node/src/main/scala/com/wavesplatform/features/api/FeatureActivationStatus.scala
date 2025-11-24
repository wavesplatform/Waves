package com.wavesplatform.features.api

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.state.Height

case class FeatureActivationStatus(
    id: Short,
    description: String,
    blockchainStatus: BlockchainFeatureStatus,
    nodeStatus: NodeFeatureStatus,
    activationHeight: Option[Height],
    supportingBlocks: Option[Int]
)
