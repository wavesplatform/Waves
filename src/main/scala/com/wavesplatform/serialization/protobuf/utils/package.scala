package com.wavesplatform.serialization.protobuf

import com.google.protobuf.{ByteString => PBByteString}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.smart.script.protobuf.{Script => PBScript}
import com.wavesplatform.transaction.smart.script.{Script, ScriptReader}
import scalapb.TypeMapper

//noinspection TypeAnnotation
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

  implicit val aliasMapper = TypeMapper[PBByteString, Alias] { bs =>
    Alias.fromBytes(bs.toByteArray).right.get
  } { alias =>
    PBByteString.copyFrom(alias.bytes.arr)
  }

  implicit val addressMapper = TypeMapper[PBByteString, Address] { bs =>
    Address.fromBytes(bs.toByteArray).right.get
  } { address =>
    PBByteString.copyFrom(address.bytes.arr)
  }

  implicit val scriptMapper = TypeMapper[PBScript, Script] { bs =>
    ScriptReader.fromBytes(bs.bytes.toByteArray).right.get
  } { script =>
    PBScript(PBByteString.copyFrom(script.bytes().arr))
  }
}
