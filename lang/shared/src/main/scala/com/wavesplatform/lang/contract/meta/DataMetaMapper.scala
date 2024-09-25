package com.wavesplatform.lang.contract.meta

import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.protobuf.dapp.DAppMeta.CompactNameAndOriginalNamePair

class DataMetaMapper(mapper: TypeBitMapper, version: MetaVersion) {
  def toProto(funcTypes: List[List[FINAL]], compactNameToOriginalNameMap: Map[String, String] = Map.empty): Either[String, DAppMeta] = {
    for {
      fTypes <- funcTypes.traverse(funcToProto)
    } yield DAppMeta(version.number, fTypes, nameMapToProto(compactNameToOriginalNameMap))
  }

  private def funcToProto(types: List[FINAL]): Either[String, CallableFuncSignature] =
    types
      .traverse(t => mapper.toIndex(t).map(_.toByte))
      .map(_.toArray)
      .map(ByteString.copyFrom)
      .map(CallableFuncSignature(_))

  private def nameMapToProto(compactNameToOriginalNameMap: Map[String, String]): Seq[CompactNameAndOriginalNamePair] = {
    compactNameToOriginalNameMap.toSeq
      .sortBy(_._1) // sort by compactName
      .map { case (k, v) =>
        CompactNameAndOriginalNamePair(k, v)
      }
  }

  def fromProto(meta: DAppMeta): Either[String, List[List[FINAL]]] =
    meta.funcs.toList.traverse(protoToFunc)

  private def protoToFunc(funcs: CallableFuncSignature): Either[String, List[FINAL]] =
    funcs.types
      .toByteArray()
      .toList
      .traverse(b => mapper.fromIndex(b.toInt))

}
