package com.wavesplatform.lang.contract.meta

import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature

class FunctionTypeMapper(mapper: TypeBitMapper, version: MetaVersion) {
  def toProto(funcTypes: List[List[FINAL]]): Either[String, DAppMeta] =
    funcTypes
      .traverse(funcToProto)
      .map(DAppMeta(version.number, _))

  private def funcToProto(types: List[FINAL]): Either[String, CallableFuncSignature] =
    types
      .traverse(t => mapper.toIndex(t).map(_.toByte))
      .map(_.toArray)
      .map(ByteString.copyFrom)
      .map(CallableFuncSignature(_))

  def fromProto(meta: DAppMeta): Either[String, List[List[FINAL]]] =
    meta.funcs.toList.traverse(protoToFunc)

  private def protoToFunc(funcs: CallableFuncSignature): Either[String, List[FINAL]] =
    funcs.types.toByteArray.toList
      .traverse(b => mapper.fromIndex(b.toInt))

}
