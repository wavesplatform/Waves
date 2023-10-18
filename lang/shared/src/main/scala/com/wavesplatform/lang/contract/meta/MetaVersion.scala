package com.wavesplatform.lang.contract.meta

sealed trait MetaVersion {
  val strategy: MetaMapperStrategy
  val number: Int
}

object V1 extends MetaVersion {
  override val strategy: MetaMapperStrategy = MetaMapperStrategyV1
  override val number: Int = 1
}

object V2 extends MetaVersion {
  override val strategy: MetaMapperStrategy = MetaMapperStrategyV2
  override val number: Int = 2
}
