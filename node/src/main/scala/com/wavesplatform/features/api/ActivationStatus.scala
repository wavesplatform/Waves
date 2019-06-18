package com.wavesplatform.features.api

import com.wavesplatform.state.Height

case class ActivationStatus(height: Height, votingInterval: Int, votingThreshold: Int, nextCheck: Int, features: Seq[FeatureActivationStatus])
