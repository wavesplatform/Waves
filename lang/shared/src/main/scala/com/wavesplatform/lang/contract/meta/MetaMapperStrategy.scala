package com.wavesplatform.lang.contract.meta

import com.wavesplatform.protobuf.dapp.DAppMeta

private[meta] trait MetaMapperStrategy[V <: MetaVersion] {
  def toProto(data: V#Data): Either[String, DAppMeta]
  def fromProto(meta: DAppMeta): Either[String, V#Data]
  def textMap(data: V#Data): Dic
  def textMapFromProto(meta: DAppMeta): Either[String, Dic] = fromProto(meta).map(textMap)
}
