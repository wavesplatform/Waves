package com.wavesplatform.lang.contract

import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.compiler.Types
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, FINAL, LONG, STRING}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import cats.implicits._
import com.wavesplatform.lang.contract.MetaMapper.{MetaVersion, V1}
import shapeless.Nat

trait MetaMapperStrategy[Version <: MetaVersion] {
  def fromProto(meta: DAppMeta): Either[String, Version#Data]
  def toProto(data: Version#Data): Either[String, DAppMeta]
}

object MetaMapperStrategyV1 extends MetaMapperStrategy[V1.type] {
  def toProto(funcTypes: V1.Data): Either[String, DAppMeta] =
    funcTypes
      .traverse { case (funcName, types) => funcToProto(funcName, types) }
      .map(DAppMeta(1, _))

  private def funcToProto(funcName: String, types: List[Types.FINAL]): Either[String, CallableFuncSignature] =
    types
      .traverse {
        case LONG    => Right(0: Byte)
        case BYTESTR => Right(1: Byte)
        case BOOLEAN => Right(2: Byte)
        case STRING  => Right(3: Byte)
        case argType => Left(s"Unexpected callable func arg type: $argType")
      }
      .map(_.toArray)
      .map(ByteString.copyFrom)
      .map(CallableFuncSignature(funcName, _))

  def fromProto(meta: DAppMeta): Either[String, Data] =
    meta.funcs.toList.traverse(protoToFunc)

  private def protoToFunc(funcs: CallableFuncSignature): Either[String, FuncArgType] = {
    val CallableFuncSignature(name, types) = funcs
    types.toByteArray.toList
      .traverse {
        case 0 => Right(LONG)
        case 1 => Right(BYTESTR)
        case 2 => Right(BOOLEAN)
        case 3 => Right(STRING)
        case n => Left(s"Unexpected callable func arg type byte: $n")
      }
      .map((name, _))
  }
}
