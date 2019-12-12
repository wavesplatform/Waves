package com.wavesplatform.lang.contract.meta

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.protobuf.dapp.DAppMeta

private[meta] trait MetaMapperStrategy[V <: MetaVersion] {
  def toProto(data: V#Data): Either[String, DAppMeta]
  def fromProto(meta: DAppMeta): Either[String, V#Data]
  def textMap(data: V#Data, dApp: DApp): Dic
  def protoInfo(dApp: DApp): Either[String, Dic] = fromProto(dApp.meta).map(textMap(_, dApp))
}
