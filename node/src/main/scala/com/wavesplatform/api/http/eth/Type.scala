package com.wavesplatform.api.http.eth

import org.web3j.abi.datatypes.{Utf8String, Type => EthType}

sealed trait Type

private[eth] case class TypeImpl(value: EthType[_]) extends Type

object Type {
  implicit def stringToE(v: String): Type = TypeImpl(new Utf8String(v))
  implicit def rawTypeToE(v: EthType[_])  = TypeImpl(v)

  def unwrap(t: Type): EthType[_] = t match {
    case TypeImpl(value) => value
  }
}
