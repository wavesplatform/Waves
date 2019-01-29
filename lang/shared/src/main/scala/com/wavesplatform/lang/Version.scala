package com.wavesplatform.lang

import supertagged._

object Version extends TaggedType[Int] {
  type Version = Version.Type

  val ExprV1: Version = 1 @@ Version
  val ExprV2: Version = 2 @@ Version

  val ContractV: Version = 3 @@ Version

  val SupportedVersions: Set[Version] = Set(ExprV1, ExprV2, ContractV)

  def parseVersion(i:Int) = i match {
    case 1 => ExprV1
    case 2 => ExprV2
    case 3 => ContractV
  }
}
