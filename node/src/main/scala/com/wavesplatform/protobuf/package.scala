package com.wavesplatform

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.transaction.Asset

package object protobuf {
  implicit final class ByteStrExt(private val bs: ByteStr) extends AnyVal {
    def toByteString: ByteString = ByteString.copyFrom(bs.arr)
  }

  implicit final class AddressExt(private val a: Address) extends AnyVal {
    def toByteString: ByteString = ByteString.copyFrom(a.bytes)
  }

  implicit final class PublicKeyExt(private val pk: PublicKey) extends AnyVal {
    def toByteString: ByteString = ByteString.copyFrom(pk.arr)
  }

  implicit final class ByteStringExt(private val bs: ByteString) extends AnyVal {
    def toByteStr: ByteStr     = ByteStr(bs.toByteArray)
    def toPublicKey: PublicKey = PublicKey(bs.toByteArray)
    def toAddress(chainId: Byte = AddressScheme.current.chainId): Address =
      PBRecipients
        .toAddress(bs.toByteArray, chainId)
        .fold(ve => throw new IllegalArgumentException(ve.toString), identity)
    def toIssuedAsset: Asset.IssuedAsset = Asset.IssuedAsset(toByteStr)
  }
}
