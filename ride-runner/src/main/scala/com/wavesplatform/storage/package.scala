package com.wavesplatform

import com.wavesplatform.account.Address

package object storage {
  type AccountDataKey    = (Address, String)
  type ActivatedFeatures = Map[Short, Int]
}
