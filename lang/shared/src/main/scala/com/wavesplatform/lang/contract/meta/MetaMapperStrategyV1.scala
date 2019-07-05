package com.wavesplatform.lang.contract.meta

import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.compiler.Types
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, FINAL, LONG, STRING, UNION}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import cats.implicits._

private[meta] object MetaMapperStrategyV1 extends MetaMapperStrategy[V1.type] {
  type FuncArgType = (String, List[FINAL])

  def toProto(funcTypes: List[FuncArgType]): Either[String, DAppMeta] =
    funcTypes
      .traverse { case (funcName, types) => funcToProto(funcName, types) }
      .map(DAppMeta(1, _))

  private def funcToProto(funcName: String, types: List[Types.FINAL]): Either[String, CallableFuncSignature] =
    types
      .traverse(mapType)
      .map(_.toArray)
      .map(ByteString.copyFrom)
      .map(CallableFuncSignature(funcName, _))

  private def mapType(t: FINAL): Either[String, Byte] = {
    val result = t match {
      case UNION(types, _) => types.traverse(mapSingleType).map(_.sum)
      case argType         => mapSingleType(argType)
    }
    result.map(_.toByte)
  }

  private def mapSingleType(t: FINAL): Either[String, Int] =
    t match {
      case LONG    => Right(1 << 0)
      case BYTESTR => Right(1 << 1)
      case BOOLEAN => Right(1 << 2)
      case STRING  => Right(1 << 3)
      case argType => Left(s"Unexpected callable func arg type: $argType")
    }

  def fromProto(meta: DAppMeta): Either[String, List[FuncArgType]] =
    meta.funcs.toList.traverse(protoToFunc)

  private def protoToFunc(funcs: CallableFuncSignature): Either[String, FuncArgType] = {
    val CallableFuncSignature(name, types) = funcs
    types.toByteArray.toList
      .traverse(buildType)
      .map((name, _))
  }

  private def buildType(b: Byte): Either[String, FINAL] = {
    if (b > 15 || b < 1) {
      Left("Illegal func arg type bytes")
    } else {
      val long    = if (((b &    1) >> 0) == 1) Some(LONG)    else None
      val byteStr = if (((b &   10) >> 1) == 1) Some(BYTESTR) else None
      val boolean = if (((b &  100) >> 2) == 1) Some(BOOLEAN) else None
      val string  = if (((b & 1000) >> 3) == 1) Some(STRING)  else None

      val existingTypes = List(long, byteStr, boolean, string).flatMap(_.toList)

      existingTypes match {
        case List(single)     => Right(single)
        case l@List(_, _@ _*) => Right(UNION(l, None))
      }
    }
  }

  override def textMap(data: List[FuncArgType]): Dic = {
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
