package com.wavesplatform

import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset

package object storage {
  type AccountDataKey    = (Address, String)
  type AccountAssetKey   = (Address, Asset)
  type ActivatedFeatures = Map[Short, Int]
}
