package com.wavesplatform.lang

import Context._
import com.wavesplatform.lang.Terms.TYPE
import monix.eval.Coeval

case class Context(typeDefs: Map[String, CustomType], defs: Defs, functions: Map[String, CustomFunction])

object Context {

  type Defs = Map[String, (TYPE, Any)]

  val empty = Context(Map.empty, Map.empty, Map.empty)

  case class CustomType(name: String, fields: List[(String, TYPE)])

  sealed trait CustomFunction {
    val resultType: TYPE
    def eval(args: List[Any]) : resultType.Underlying
  }
  object CustomFunction {

    case class CustomFunctionImpl(name: String, resultType: TYPE, args: List[(String, TYPE)], ev: List[Any] => Any) extends CustomFunction {
      override def eval(args: List[Any]): resultType.Underlying = {
        ev(args).asInstanceOf[resultType.Underlying]
      }
    }

    def apply(name: String, resultType: TYPE, args: List[(String, TYPE)])(ev: List[Any] => resultType.Underlying): CustomFunction
        = CustomFunctionImpl(name, resultType, args, ev)

  }

  sealed trait LazyVal {
    val tpe: TYPE
    val value: Coeval[tpe.Underlying]
  }

  object LazyVal {
    private case class LazyValImpl(tpe: TYPE, v: Coeval[Any]) extends LazyVal {
      override val value: Coeval[tpe.Underlying] = v.map(_.asInstanceOf[tpe.Underlying])
    }

    def apply(t: TYPE)(v: Coeval[t.Underlying]): LazyVal = LazyValImpl(t, v)
  }

  case class Obj(fields: Map[String, LazyVal])


}