package com.wavesplatform.features.api

import com.wavesplatform.features.BlockchainFeatureStatus

case class ActivationStatusFeature(id: Short,
                                   blockchainStatus: BlockchainFeatureStatus,
                                   nodeStatus: NodeFeatureStatus,
                                   activationHeight: Option[Int],
                                   supportedBlocks: Option[Int])
