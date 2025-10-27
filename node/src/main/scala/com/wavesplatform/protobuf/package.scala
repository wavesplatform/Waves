package com.wavesplatform

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.TransactionId
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

import scala.annotation.targetName

package object protobuf {
  extension (bs: ByteStr) def toByteString: ByteString = ByteString.copyFrom(bs.arr)

  extension (txId: TransactionId) {
    @targetName("txIdToByteString") def toByteString: ByteString = ByteString.copyFrom(txId.arr)
  }

  extension (a: Address) def toByteString: ByteString = ByteString.copyFrom(a.bytes)

  extension (pk: PublicKey) {
    @targetName("publicKeyToByteString") def toByteString: ByteString = ByteString.copyFrom(pk.arr)
  }

  extension (bs: ByteString) {
    def toByteStr: ByteStr           = ByteStr(bs.toByteArray)
    def toTxId: TransactionId        = TransactionId(toByteStr)
    def toIssuedAssetId: IssuedAsset = IssuedAsset(ByteStr(bs.toByteArray))
    def toAssetId: Asset             = if (bs.isEmpty) Waves else toIssuedAssetId
    def toPublicKey: PublicKey       = PublicKey(bs.toByteArray)
    def toAddress(chainId: Byte = AddressScheme.current.chainId): Address =
      PBRecipients
        .toAddress(bs.toByteArray, chainId)
        .fold(ve => throw new IllegalArgumentException(ve.toString), identity)
    def toIssuedAsset: Asset.IssuedAsset = Asset.IssuedAsset(toByteStr)
  }
}
