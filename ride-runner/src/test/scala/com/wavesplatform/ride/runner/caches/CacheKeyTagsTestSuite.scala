package com.wavesplatform.ride.runner.caches

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.transaction.Asset.IssuedAsset

class CacheKeyTagsTestSuite extends BaseTestSuite {
  "CacheKeyTags" - {
    "removing tags" in {
      val tags = new CacheKeyTags[Int]
      (1 to 3).foreach(tags.addDependent(MemCacheKey.Height, _))

      val btcKey = MemCacheKey.Asset(IssuedAsset(ByteStr.decodeBase58("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get))
      (1 to 2).foreach(tags.addDependent(btcKey, _))

      tags.removeTags(Set(1, 2))
      tags.get(btcKey) shouldBe None
      tags.get(MemCacheKey.Height) shouldBe Some(Set(3))
    }
  }
}
