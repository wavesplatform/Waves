package com.wavesplatform.lang

import supertagged._

object Version extends TaggedType[Int] {
  type Version = Version.Type

  val V1: Version = 1 @@ Version
  val V2: Version = 2 @@ Version

  val V3: Version = 3 @@ Version

  val SupportedVersions: Set[Version] = Set(V1, V2, V3)

  def fromInt(i:Int) = i match {
    case 1 => V1
    case 2 => V2
    case 3 => V3
  }
}
