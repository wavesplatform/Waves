package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.derivation.default.*

case class RewardsVotingSettings(desired: Option[Long]) derives ConfigReader
