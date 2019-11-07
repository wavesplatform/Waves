package com.wavesplatform

import java.nio.ByteBuffer

import com.google.common.primitives.Shorts
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.transaction.{Asset, Proofs}

package object serialization {
  implicit class ByteBufferOps(private val buf: ByteBuffer) extends AnyVal {
    def getPrefixedByteArray: Array[Byte] = {
      val prefix = buf.getShort
      require(prefix >= 0, "negative array length")
      if (prefix > 0) getByteArray(prefix) else Array.emptyByteArray
    }

    def getAsset: Asset = {
      val prefix = buf.get
      if (prefix == 0) Asset.Waves
      else if (prefix == 1) Asset.IssuedAsset(ByteStr(getByteArray(transaction.AssetIdLength)))
      else throw new IllegalArgumentException(s"Invalid asset id prefix: $prefix")
    }

    def getAddressOrAlias: AddressOrAlias = {
      val prefix = buf.get(buf.position())
      prefix match {
        case Address.AddressVersion =>
          Address.fromBytes(getByteArray(Address.AddressLength)).explicitGet()
        case Alias.AddressVersion =>
          val length = buf.getShort(buf.position() + 2)
          Alias.fromBytes(getByteArray(length + 4)).explicitGet()
        case _ => throw new IllegalArgumentException(s"Invalid address or alias prefix: $prefix")
      }
    }

    def getByteArray(size: Int): Array[Byte] = {
      val result = new Array[Byte](size)
      buf.get(result)
      result
    }

    def getShortArray(size: Int): Array[Short] = {
      val result = new Array[Short](size)
      buf.asShortBuffer().get(result)
      buf.position(buf.position() + Shorts.BYTES * size)
      result
    }

    def getSignature: Array[Byte] = getByteArray(SignatureLength)

    def getPublicKey: PublicKey = PublicKey(getByteArray(KeyLength))

    def getProofs: Proofs = Proofs.fromBytes(buf.getByteArray(buf.remaining())).explicitGet()

    def getAlias: Alias = Alias.fromBytes(buf.getPrefixedByteArray).explicitGet()
  }
}
