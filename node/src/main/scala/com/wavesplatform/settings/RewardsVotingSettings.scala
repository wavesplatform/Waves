package com.wavesplatform.settings

import pureconfig.*

case class RewardsVotingSettings(desired: Option[Long]) derives ConfigReader
