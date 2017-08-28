package com.wavesplatform.settings

import scala.concurrent.duration.Duration


case class MinerSettings(
  enable: Boolean,
  quorum: Int,
  intervalAfterLastBlockThenGenerationIsAllowed: Duration)
