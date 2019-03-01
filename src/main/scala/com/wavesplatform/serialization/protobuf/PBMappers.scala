package com.wavesplatform.serialization.protobuf

import com.google.protobuf.{ByteString => PBByteString}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.transaction.protobuf.PBAssetId
import com.wavesplatform.transaction.smart.script.protobuf.{Script => PBScript}
import com.wavesplatform.transaction.smart.script.{Script, ScriptReader}
import scalapb.TypeMapper

trait PBMappers {
  // TODO: Remove byte arrays copying with reflection
  implicit val byteStringMapper: TypeMapper[PBByteString, ByteStr] = TypeMapper[PBByteString, ByteStr] { bs ⇒
    if (bs.isEmpty) ByteStr.empty else ByteStr(bs.toByteArray)
  } { bs ⇒
    if (bs.isEmpty) PBByteString.EMPTY else PBByteString.copyFrom(bs.arr)
  }

  implicit val publicKeyAccountMapper = TypeMapper[PBByteString, PublicKeyAccount] { bs =>
    if (bs.isEmpty) PublicKeyAccount(Array.emptyByteArray) else PublicKeyAccount(bs.toByteArray)
  } { pka =>
    PBByteString.copyFrom(pka.publicKey)
  }

  implicit val addressMapper = TypeMapper[PBByteString, Address] { bs =>
    if (bs.isEmpty) Address.createUnsafe(Array.emptyByteArray) else Address.fromBytes(bs.toByteArray).explicitGet()
  } { pka =>
    PBByteString.copyFrom(pka.bytes.arr)
  }

  implicit val assetIdMapper = TypeMapper[PBByteString, PBAssetId] { bs =>
    if (bs.isEmpty) PBAssetId.Waves else PBAssetId(bs.toByteArray)
  } { assetId =>
    PBByteString.copyFrom(assetId.bytes)
  }

  implicit val scriptMapper = TypeMapper[PBScript, Script] { bs =>
    ScriptReader.fromBytes(bs.bytes.toByteArray).explicitGet()
  } { script =>
    PBScript(PBByteString.copyFrom(script.bytes().arr))
  }

  /* implicit val chainIdMapper = TypeMapper[PBByteString, ChainId] { bs =>
    if (bs.isEmpty) ChainId.empty else ChainId.fromByte(bs.byteAt(0))
  } { chainId =>
    if (chainId.isEmpty) PBByteString.EMPTY else PBByteString.copyFrom(Array(chainId.byte))
  } */

  implicit val byteToBytesMapper = TypeMapper[PBByteString, Byte] { bs =>
    if (bs.isEmpty) 0 else bs.byteAt(0)
  } { byte =>
    if (byte == 0) PBByteString.EMPTY else PBByteString.copyFrom(Array(byte))
  }

  implicit def byteStringToByteStr(bs: PBByteString): ByteStr = bs.toByteArray
  implicit def byteStrToByteString(bs: ByteStr): PBByteString = PBByteString.copyFrom(bs)

  // implicit def implicitPBMapTypeToCustom[BT, CT](value: BT)(implicit ev: TypeMapper[BT, CT]): CT = ev.toCustom(value)
  // implicit def implicitPBMapTypeToBase[CT, BT](value: CT)(implicit ev: TypeMapper[BT, CT]): BT   = ev.toBase(value)

  implicit class PBByteStringOps(bs: PBByteString) {
    def byteStr          = ByteStr(bs.toByteArray)
    def publicKeyAccount = PublicKeyAccount(bs.toByteArray)
  }
}
