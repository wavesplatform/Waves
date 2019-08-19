package com.wavesplatform.lang.contract.meta

import com.wavesplatform.lang.v1.compiler.Types.FINAL

sealed trait MetaVersion {
  type Self <: MetaVersion
  type Data
  val strategy: MetaMapperStrategy[Self]
}

object V1 extends MetaVersion {
  override type Self = V1.type
  override type Data = List[List[FINAL]]
  override val strategy = MetaMapperStrategyV1
}
