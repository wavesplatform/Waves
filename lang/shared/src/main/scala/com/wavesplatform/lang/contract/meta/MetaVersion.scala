package com.wavesplatform.lang.contract.meta

sealed trait MetaVersion {
  type Self <: MetaVersion
  val strategy: MetaMapperStrategy[Self]
  val number: Int
}

object V1 extends MetaVersion {
  override type Self = V1.type
  override val strategy: MetaMapperStrategy[Self] = MetaMapperStrategyV1
  override val number: Int = 1
}

object V2 extends MetaVersion {
  override type Self = V2.type
  override val strategy: MetaMapperStrategy[Self] = MetaMapperStrategyV2
  override val number: Int = 2
}
