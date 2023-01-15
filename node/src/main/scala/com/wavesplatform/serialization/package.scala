package com.wavesplatform

import java.nio.ByteBuffer
import com.google.common.primitives.Shorts
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order

package object serialization {
  implicit class ByteBufferOps(private val buf: ByteBuffer) extends AnyVal {
    def getByteArrayWithLength: Array[Byte] = {
      val prefix = buf.getShort
      require(prefix >= 0, "negative array length")
      if (prefix > 0) getByteArray(prefix) else Array.emptyByteArray
    }

    def getIssuedAsset: IssuedAsset =
      Asset.IssuedAsset(ByteStr(getByteArray(transaction.AssetIdLength)))

    def getAsset: Asset = buf.getByte match {
      case 0 => Asset.Waves
      case 1 => this.getIssuedAsset
      case b => throw new IllegalArgumentException(s"Invalid asset id prefix: $b")
    }

    def getAddressOrAlias(chainId: Byte = AddressScheme.current.chainId): AddressOrAlias = {
      val prefix = buf.get(buf.position())
      prefix match {
        case Address.AddressVersion =>
          getAddress(chainId)
        case Alias.AddressVersion =>
          val length = buf.getShort(buf.position() + 2)
          Alias.fromBytes(getByteArray(length + 4)).explicitGet()
        case _ => throw new IllegalArgumentException(s"Invalid address or alias prefix: $prefix")
      }
    }

    def getAddress(chainId: Byte = AddressScheme.current.chainId): Address = {
      Address.fromBytes(getByteArray(Address.AddressLength), chainId).explicitGet()
    }

    // More explicit name
    def getByte: Byte =
      buf.get()

    def getBoolean: Boolean = getByte match {
      case 0 => false
      case 1 => true
      case b => throw new IllegalArgumentException(s"Invalid boolean value: $b")
    }

    def getByteArray(size: Int): Array[Byte] = {
      require(size < (10 << 20), s"requested array size $size exceeds 10MB limit")
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

    def getSignature: ByteStr = ByteStr(getByteArray(SignatureLength))

    def getPublicKey: PublicKey = PublicKey(getByteArray(KeyLength))

    def getProofs: Proofs = Proofs.fromBytes(buf.getByteArray(buf.remaining())).explicitGet()

    def getScript: Option[Script] = Deser.parseByteArrayOptionWithLength(buf).map(ScriptReader.fromBytes(_).explicitGet())

    def getAlias: Alias = Alias.fromBytes(buf.getByteArrayWithLength).explicitGet()

    def getVersionedOrder: Order = {
      val length  = buf.getInt
      val version = buf.get()
      version match {
        case 1 =>
          Order.parseBytes(1.toByte, buf.getByteArray(length)).get

        case 2 | 3 =>
          val outArray = new Array[Byte](length)
          outArray(0) = version
          buf.get(outArray, 1, length - 1)
          Order.parseBytes(version, outArray).get

        case _ =>
          throw new IllegalArgumentException(s"Invalid order version: $version")
      }
    }
  }
}
