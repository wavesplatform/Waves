package com.wavesplatform.lang.contract.meta

sealed trait RecKeyValue
case class Single(s: String)                extends RecKeyValue
case class Chain(l: List[RecKeyValue])      extends RecKeyValue
case class Dic(m: Map[String, RecKeyValue]) extends RecKeyValue

