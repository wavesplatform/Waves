package com.wavesplatform.lang

import Context._
import cats.data.EitherT
import com.wavesplatform.lang.Terms.TYPE
import monix.eval.Coeval

case class Context(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[String, PredefFunction])

object Context {

  val empty = Context(Map.empty, Map.empty, Map.empty)

  case class PredefType(name: String, fields: List[(String, TYPE)])

  sealed trait PredefFunction {
    val name: String
    val args: List[(String, TYPE)]
    val resultType: TYPE
    def eval(args: List[Any]): TrampolinedExecResult[resultType.Underlying]
    val signature: (List[TYPE], TYPE)
  }
  object PredefFunction {

    case class PredefFunctionImpl(name: String, resultType: TYPE, args: List[(String, TYPE)], ev: List[Any] => Either[String, Any])
        extends PredefFunction {
      override def eval(args: List[Any]): TrampolinedExecResult[resultType.Underlying] = {
        EitherT.fromEither[Coeval](ev(args).map(_.asInstanceOf[resultType.Underlying]))
      }
      override lazy val signature: (List[TYPE], TYPE) = (args.map(_._2), resultType)
    }

    def apply(name: String, resultType: TYPE, args: List[(String, TYPE)])(ev: List[Any] => Either[String, resultType.Underlying]): PredefFunction =
      PredefFunctionImpl(name, resultType, args, ev)

  }

  sealed trait LazyVal {
    val tpe: TYPE
    val value: TrampolinedExecResult[tpe.Underlying]
  }

  object LazyVal {
    private case class LazyValImpl(tpe: TYPE, v: TrampolinedExecResult[Any]) extends LazyVal {
      override val value: TrampolinedExecResult[tpe.Underlying] = EitherT(Coeval.evalOnce(v.map(_.asInstanceOf[tpe.Underlying]).value.apply()))
    }

    def apply(t: TYPE)(v: TrampolinedExecResult[t.Underlying]): LazyVal = LazyValImpl(t, v.map(_.asInstanceOf[Any]))
  }

  case class Obj(fields: Map[String, LazyVal])

}
