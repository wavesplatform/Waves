package com.wavesplatform

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.TransactionId
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

package object protobuf {
  implicit class ByteStrExt(val bs: ByteStr) extends AnyVal {
    def toByteString: ByteString = ByteString.copyFrom(bs.arr)
  }

  implicit class AddressExt(val a: Address) extends AnyVal {
    def toByteString: ByteString = ByteString.copyFrom(a.bytes)
  }

  implicit class ByteStringExt(val bs: ByteString) extends AnyVal {
    def toByteStr: ByteStr           = ByteStr(bs.toByteArray)
    def toTxId: TransactionId        = TransactionId @@ toByteStr
    def toIssuedAssetId: IssuedAsset = IssuedAsset(ByteStr(bs.toByteArray))
    def toAssetId: Asset             = if (bs.isEmpty) Waves else toIssuedAssetId
    def toPublicKey: PublicKey       = PublicKey(bs.toByteArray)
    def toAddress: Address =
      PBRecipients
        .toAddress(bs.toByteArray, AddressScheme.current.chainId)
        .fold(ve => throw new IllegalArgumentException(ve.toString), identity)
  }
}
