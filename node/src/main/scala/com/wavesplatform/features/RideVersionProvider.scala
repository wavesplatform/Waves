package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatures.{BlockV5, ConsensusImprovements, Ride4DApps, RideV6, SynchronousCalls, TransactionStateSnapshot}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.state.Blockchain

object RideVersionProvider {
  val actualVersionByFeature =
    List(
      TransactionStateSnapshot -> V8,
      ConsensusImprovements    -> V7, // TODO: fix after version-1.4.x merge
      RideV6                   -> V6,
      SynchronousCalls         -> V5,
      BlockV5                  -> V4,
      Ride4DApps               -> V3
    )

  DirectiveDictionary[StdLibVersion].all
    .filter(_ >= V3)
    .foreach { v =>
      if (!actualVersionByFeature.map(_._2).contains(v))
        throw new RuntimeException(s"Blockchain feature related to RIDE $v is not found")
    }

  implicit class RideVersionBlockchainExt(b: Blockchain) {
    def actualRideVersion: StdLibVersion =
      actualVersionByFeature
        .collectFirst { case (feature, version) if b.isFeatureActivated(feature) => version }
        .getOrElse(V3)
  }
}
