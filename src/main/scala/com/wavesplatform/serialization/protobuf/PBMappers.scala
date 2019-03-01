package com.wavesplatform.serialization.protobuf

import com.google.protobuf.{ByteString => PBByteString}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr

trait PBMappers {
  implicit def byteStringToByteStr(bs: PBByteString): ByteStr = bs.toByteArray
  implicit def byteStrToByteString(bs: ByteStr): PBByteString = PBByteString.copyFrom(bs)

  // implicit def implicitPBMapTypeToCustom[BT, CT](value: BT)(implicit ev: TypeMapper[BT, CT]): CT = ev.toCustom(value)
  // implicit def implicitPBMapTypeToBase[CT, BT](value: CT)(implicit ev: TypeMapper[BT, CT]): BT   = ev.toBase(value)

  implicit class PBByteStringOps(bs: PBByteString) {
    def byteStr          = ByteStr(bs.toByteArray)
    def publicKeyAccount = PublicKeyAccount(bs.toByteArray)
  }
}
