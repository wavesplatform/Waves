package com.wavesplatform.ride.runner

import supertagged.TaggedType

package object caches {
  object DbKeyIndex extends TaggedType[Int]
  type DbKeyIndex = DbKeyIndex.Type
}
