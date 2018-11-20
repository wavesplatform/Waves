package com.wavesplatform.lang

import supertagged._

object Version extends TaggedType[Int] {
  type Version = Version.Type

  val V1: Version = 1 @@ Version
  val V2: Version = 2 @@ Version

  val SupportedVersions: Set[Version] = Set(V1, V2)
}
