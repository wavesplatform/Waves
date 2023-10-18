package com.wavesplatform.lang.contract.meta

import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.protobuf.dapp.DAppMeta

private[meta] trait MetaMapperStrategy {
  def toProto(data: List[List[FINAL]], nameMap: Map[String, String] = Map.empty): Either[String, DAppMeta]
  def fromProto(meta: DAppMeta): Either[String, List[List[FINAL]]]
}
