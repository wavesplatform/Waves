package com.wavesplatform.matcher.market

import com.wavesplatform.NoShrink
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpecLike}

class SnapshotStateSpecification extends PropSpecLike with GeneratorDrivenPropertyChecks with Matchers with NoShrink {
  property("nextSnapshotOffset generates greater offsets than old and last processed") {
    val assetPair = AssetPair(
      Some(ByteStr("asset1".getBytes())),
      Some(ByteStr("asset2".getBytes()))
    )

    val g = for {
      interval            <- Gen.choose(1, 1000L).label("interval")
      currSnapshotOffset  <- Gen.choose(-1, 1000L).label("currSnapshotOffset")
      lastProcessedOffset <- Gen.choose(-1, 1000L).label("lastProcessedOffset")
    } yield (currSnapshotOffset, lastProcessedOffset, interval)

    forAll(g) {
      case (currSnapshotOffset, lastProcessedOffset, interval) =>
        val nextOffset = SnapshotsState.nextSnapshotOffset(assetPair, currSnapshotOffset, lastProcessedOffset, interval)
        nextOffset should be > currSnapshotOffset
        nextOffset should be > lastProcessedOffset
    }
  }
}
