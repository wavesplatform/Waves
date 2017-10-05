package com.wavesplatform.features.api

case class ActivationStatus(height: Int,
                            votingInterval: Int,
                            votingThreshold: Int,
                            nextCheck: Int,
                            features: Set[ActivationStatusFeature])
