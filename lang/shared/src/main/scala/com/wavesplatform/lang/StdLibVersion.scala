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

object ContentType extends TaggedType[Int] {
  type ContentType = ContentType.Type

  val Expression: ContentType = 1 @@ ContentType
  val Contract: ContentType   = 2 @@ ContentType

  val SupportedTypes: Set[ContentType] = Set(Expression, Contract)

  def parseId(i: Int) = i match {
    case 1 => Expression
    case 2 => Contract
  }

  def isContract(isContract: Boolean) = if (isContract) Contract else Expression

  def parseString(s: String) = s match {
    case "EXPRESSION" => Expression
    case "CONTRACT"   => Contract
  }
}

object ScriptType extends TaggedType[Int] {
  type ScriptType = ScriptType.Type

  val Account: ScriptType = 1 @@ ScriptType
  val Asset: ScriptType   = 2 @@ ScriptType

  val SupportedTypes: Set[ScriptType] = Set(Account, Asset)

  def parseId(i: Int) = i match {
    case 1 => Account
    case 2 => Asset
  }

  def isAssetScript(b: Boolean) = if (b) Asset else Account

  def parseString(s: String) = s match {
    case "ACCOUNT" => Account
    case "ASSET"   => Asset
  }
}
