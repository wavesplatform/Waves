package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.TYPE

case class Obj(fields: Map[String, LazyVal])

sealed trait Val {
  val tpe: TYPE
  val value: tpe.Underlying
}

object Val {
  private case class ValImpl(tpe: TYPE, v: Any) extends Val {
    override val value: tpe.Underlying = v.asInstanceOf[tpe.Underlying]
  }

  def apply(t: TYPE)(v: t.Underlying): Val = ValImpl(t, v)
}

case class CaseObj(fields: Map[String, Val])
