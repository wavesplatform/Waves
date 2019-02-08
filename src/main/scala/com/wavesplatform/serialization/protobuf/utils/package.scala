package com.wavesplatform.serialization.protobuf
import com.google.protobuf.{ByteString => PBByteString}
import com.wavesplatform.account.{AddressOrAlias, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import scalapb.TypeMapper

package object utils {
  // TODO: Remove byte arrays copying with reflection
  implicit val byteStringMapper = TypeMapper[PBByteString, ByteStr] { bs ⇒
    if (bs.isEmpty) ByteStr.empty else ByteStr(bs.toByteArray)
  } { bs ⇒
    if (bs.isEmpty) PBByteString.EMPTY else PBByteString.copyFrom(bs.arr)
  }

  implicit val publicKeyAccountMapper = TypeMapper[PBByteString, PublicKeyAccount] { bs =>
    PublicKeyAccount(bs.toByteArray)
  } { pka =>
    PBByteString.copyFrom(pka.publicKey)
  }

  implicit val addressOrAliasMapper = TypeMapper[PBByteString, AddressOrAlias] { bs =>
    AddressOrAlias.fromBytes(bs.toByteArray, 0).right.get._1
  } { addressOrAlias =>
    PBByteString.copyFrom(addressOrAlias.bytes.arr)
  }
}
