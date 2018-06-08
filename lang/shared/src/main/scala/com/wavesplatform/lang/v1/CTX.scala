package com.wavesplatform.lang.v1

import cats.Monoid
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.Types.TYPE
import com.wavesplatform.lang.v1.evaluator.ctx.{DefinedType, EvaluationContext, LazyVal, PredefFunction}

case class CTX(types: Seq[DefinedType], vars: Map[String, (TYPE, LazyVal)], functions: Seq[PredefFunction]) {
  lazy val evaluationContext: EvaluationContext = {
    if (functions.map(_.header).distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(letDefs = vars.mapValues(_._2), functions = functions.map(f => f.header -> f).toMap)
  }
  lazy val compilerContext: CompilerContext = CompilerContext(
    predefTypes = types.map(t => t.name -> t).toMap,
    varDefs = vars.mapValues(_._1),
    functionDefs = functions.groupBy(_.name).map { case (k, v) => k -> v.map(_.signature).toList }
  )
}
object CTX {
  val empty = CTX(Seq.empty, Map.empty, Seq.empty)

  implicit val monoid: Monoid[CTX] = new Monoid[CTX] {
    override val empty: CTX                   = CTX.empty
    override def combine(x: CTX, y: CTX): CTX = CTX(x.types ++ y.types, x.vars ++ y.vars, x.functions ++ y.functions)
  }
}
