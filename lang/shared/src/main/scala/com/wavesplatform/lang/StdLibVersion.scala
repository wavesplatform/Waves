package com.wavesplatform.lang

import supertagged._

object StdLibVersion extends TaggedType[Int] {
  type StdLibVersion = StdLibVersion.Type

  val V1: StdLibVersion = 1 @@ StdLibVersion
  val V2: StdLibVersion = 2 @@ StdLibVersion
  val V3: StdLibVersion = 3 @@ StdLibVersion

  val SupportedVersions: Set[StdLibVersion] = Set(V1, V2, V3)

  def parseVersion(i: Int) = i match {
    case 1 => V1
    case 2 => V2
    case 3 => V3
  }
}

object ScriptType extends TaggedType[Int] {
  type ScriptType = ScriptType.Type

  val Expression: ScriptType = 1 @@ ScriptType
  val Contract: ScriptType = 2 @@ ScriptType

  val SupportedVersions: Set[ScriptType] = Set(Expression, Contract)

  def parseVersion(i: Int) = i match {
    case 1 => Expression
    case 2 => Contract
  }

  def parseString(s: String) = s match {
    case "EXPRESSION" => Expression
    case "CONTRACT" => Contract
  }
}
