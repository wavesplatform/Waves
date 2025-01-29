package com.wavesplatform.generator

import pureconfig.generic.derivation.EnumConfigReader

enum Mode derives EnumConfigReader {
  case WIDE, NARROW, DYN_WIDE, MULTISIG, ORACLE, SWARM
}
