package com.wavesplatform.lang.contract

import com.wavesplatform.lang.v1.compiler.Types
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.protobuf.dapp.DAppMeta
import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature

object MetaMapper {
  type FuncArgTypes = (String, List[FINAL])

  def toProto(funcTypes: List[FuncArgTypes]): Either[String, DAppMeta] =
    funcTypes
      .traverse { case (funcName, types) => funcToProto(funcName, types) }
      .map(DAppMeta(_))

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

  def fromProto(meta: DAppMeta): Either[String, List[FuncArgTypes]] =
    meta.funcs.toList.traverse(protoToFunc)

  private def protoToFunc(funcs: CallableFuncSignature): Either[String, FuncArgTypes] = {
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
