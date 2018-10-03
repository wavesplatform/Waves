package com.wavesplatform.generator

object Mode extends Enumeration {
  type Mode = Value
  val WIDE, NARROW, DYN_WIDE, MULTISIG, ORACLE, SWARM = Value
}
