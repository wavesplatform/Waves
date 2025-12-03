package com.wavesplatform.generator

import pureconfig.generic.derivation.EnumConfigReaderDerivation

enum Mode derives ECD.EnumConfigReader {
  case WIDE, NARROW, DYN_WIDE, MULTISIG, ORACLE, SWARM
}

object ECD extends EnumConfigReaderDerivation(identity)
