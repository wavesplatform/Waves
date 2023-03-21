package com.wavesplatform.riderunner

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.transaction.Asset
import supertagged.TaggedType

package object storage {
  type AccountDataKey    = (Address, String)
  type AccountAssetKey   = (Address, Asset)
  type ActivatedFeatures = Map[Short, Int]

  object DbKeyIndex extends TaggedType[Int]
  type DbKeyIndex = DbKeyIndex.Type

  def toVanillaAddress(bs: ByteString, chainId: Byte): Address =
    PBRecipients.toAddress(bs.toByteArray, chainId).fold(ve => throw new IllegalArgumentException(ve.toString), identity)
}
