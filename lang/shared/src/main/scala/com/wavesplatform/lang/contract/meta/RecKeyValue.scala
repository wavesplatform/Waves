package com.wavesplatform.lang.contract.meta

sealed trait RecKeyValue
case class Str(s: String)                   extends RecKeyValue
case class Bool(b: Boolean)                 extends RecKeyValue
case class Chain(l: List[RecKeyValue])      extends RecKeyValue
case class Dic(m: Map[String, RecKeyValue]) extends RecKeyValue

case class RecKeyValueFolder[V, R <% V](
    fromStr: String => V,
    fromBool: Boolean => V,
    fromList: List[V] => V,
    fromDic: Seq[(String, V)] => R
) {
  def fold(rkv: RecKeyValue): V =
    rkv match {
      case Str(s)   => fromStr(s)
      case Bool(b)  => fromBool(b)
      case Chain(l) => fromList(l.map(fold))
      case m: Dic   => foldRoot(m)
    }

  def foldRoot(dic: Dic): R = {
    val result = dic.m.mapValues(fold)
    fromDic(result.toSeq)
  }
}
