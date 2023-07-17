package com.wavesplatform.api.http.eth

import org.web3j.abi.datatypes.{Utf8String, Type as EthType}

sealed trait Type

private[eth] case class TypeImpl(value: EthType[?]) extends Type

object Type {
  implicit def stringToE(v: String): Type          = TypeImpl(new Utf8String(v))
  implicit def rawTypeToE(v: EthType[?]): TypeImpl = TypeImpl(v)

  def unwrap(t: Type): EthType[?] = t match {
    case TypeImpl(value) => value
  }
}
