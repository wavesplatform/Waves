package com.wavesplatform.lang.contract

import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.compiler.Types
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, FINAL, LONG, STRING}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import cats.implicits._

trait MetaMapperStrategy {
  type Data
  def toProto(data: Data): Either[String, DAppMeta]
  def fromProto(meta: DAppMeta): Either[String, Data]
  def textMap(data: Data): Dic
  def textMapFromProto(meta: DAppMeta): Either[String, Dic] = fromProto(meta).map(textMap)
}

object MetaMapperStrategyV1 extends MetaMapperStrategy {
  type FuncArgType = (String, List[FINAL])
  override type Data = List[FuncArgType]

  def toProto(funcTypes: Data): Either[String, DAppMeta] =
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

  override def textMap(data: Data): Dic = {
    val funcTypesJson = data.map { case (name, types) =>
      Dic(
        Map(
          "name"  -> Single(name),
          "types" -> Chain(types.map(_.name).map(Single))
        )
      )
    }
    Dic(Map("callableFuncTypes" -> Chain(funcTypesJson)))
  }
}
