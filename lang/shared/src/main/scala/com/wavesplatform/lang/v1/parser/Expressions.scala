package com.wavesplatform.lang.v1.parser

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Types.*

object Expressions {

  sealed trait Pos {
    def end: Int
    def start: Int
    override def toString: String = ""
  }

  object Pos {
    def apply(start: Int, end: Int)(implicit offset: Int): Pos = RealPos(start - offset, end - offset)

    override def equals(obj: scala.Any): Boolean = super.equals(obj)

    final case class RealPos(start: Int, end: Int) extends Pos { self =>
      override def equals(obj: scala.Any): Boolean = obj match {
        case AnyPos        => true
        case RealPos(s, e) => s == start && e == end
        case _             => false
      }
    }
    case object AnyPos extends Pos {
      override def equals(obj: scala.Any): Boolean = obj match {
        case _: Pos => true
        case _      => false
      }

      override def start: Int = -1
      override def end: Int   = -1
    }
  }

  trait Positioned {
    def position: Pos
  }

  trait Typed {
    def resultType: Option[FINAL]
  }

  trait ContextContainer {
    def ctxOpt: CtxOpt
  }

  sealed trait PART[+T] extends Positioned
  object PART {
    case class VALID[T](position: Pos, v: T) extends PART[T] {
      override def equals(obj: Any): Boolean =
        obj match {
          case VALID(_, value) => value == this.v
          case _               => false
        }

      override def hashCode: Int    = v.hashCode
      override def toString: String = v.toString
    }
    case class INVALID(position: Pos, message: String) extends PART[Nothing]

    def toEither[T](part: PART[T]): Either[String, T] = part match {
      case Expressions.PART.VALID(_, x)         => Right(x)
      case Expressions.PART.INVALID(p, message) => Left(message)
    }

    def toOption[T](part: PART[T]): Option[T] = part match {
      case Expressions.PART.VALID(_, x)         => Some(x)
      case Expressions.PART.INVALID(p, message) => None
    }
  }

  sealed trait Declaration extends Positioned {
    def position: Pos
    def name: PART[String]
    def allowShadowing: Boolean
  }

  case class LET(
      position: Pos,
      name: PART[String],
      value: EXPR,
      types: Option[FINAL] = None,
      allowShadowing: Boolean = false
  ) extends Declaration

  sealed trait Type {
    def isEmpty: Boolean =
      this match {
        case _: Single    => false
        case AnyType(_)   => false
        case Union(types) => types.isEmpty
        case Tuple(types) => types.exists(_.isEmpty)
      }
  }
  object Type {
    val ListTypeName = "List"
  }
  case class Single(name: PART[String], parameter: Option[PART[Type]] = None) extends Type
  case class AnyType(position: Pos)                                           extends Type
  case class Union(types: Seq[Type])                                          extends Type
  object Union {
    def apply(types: Seq[Type]): Type = types match {
      case Seq(t) => t
      case _      => new Union(types)
    }
  }
  case class Tuple(types: Seq[Type]) extends Type

  type CtxOpt = Option[Map[String, Pos]]

  case class FUNC(position: Pos, expr: EXPR, name: PART[String], args: Seq[(PART[String], Type)]) extends Declaration {
    val allowShadowing = false
  }

  case class ANNOTATION(position: Pos, name: PART[String], args: Seq[PART[String]]) extends Positioned
  case class ANNOTATEDFUNC(position: Pos, anns: Seq[ANNOTATION], f: FUNC) extends Positioned {
    def name: PART[String] = f.name
  }

  sealed trait EXPR extends Positioned with Typed with ContextContainer {
    def getName: String = this.getClass().getSimpleName
  }
  case class CONST_LONG(position: Pos, value: Long, ctxOpt: CtxOpt = None) extends EXPR {
    val resultType: Option[FINAL] = Some(LONG)
  }

  case class GETTER(
      position: Pos,
      ref: EXPR,
      field: PART[String],
      resultType: Option[FINAL] = None,
      ctxOpt: CtxOpt = None,
      checkObjectType: Boolean = true
  ) extends EXPR

  case class CONST_BYTESTR(position: Pos, value: PART[ByteStr], ctxOpt: CtxOpt = None) extends EXPR {
    val resultType: Option[FINAL] = Some(BYTESTR)
  }
  case class CONST_STRING(position: Pos, value: PART[String], ctxOpt: CtxOpt = None) extends EXPR {
    val resultType: Option[FINAL] = Some(STRING)
  }
  case class BINARY_OP(position: Pos, a: EXPR, kind: BinaryOperation, b: EXPR, resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None) extends EXPR
  case class BLOCK(position: Pos, let: Declaration, body: EXPR, resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None)                extends EXPR
  case class IF(position: Pos, cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None)        extends EXPR
  case class REF(position: Pos, key: PART[String], resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None)                             extends EXPR
  case class TRUE(position: Pos, ctxOpt: CtxOpt = None) extends EXPR {
    val resultType: Option[FINAL] = Some(BOOLEAN)
  }
  case class FALSE(position: Pos, ctxOpt: CtxOpt = None) extends EXPR {
    val resultType: Option[FINAL] = Some(BOOLEAN)
  }
  case class FUNCTION_CALL(position: Pos, name: PART[String], args: List[EXPR], resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None) extends EXPR

  case class GENERIC_FUNCTION_CALL(position: Pos, expr: EXPR, name: PART[String], `type`: Type, resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None) extends EXPR

  case class FOLD(position: Pos, limit: Int, list: EXPR, acc: EXPR, func: REF, resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None) extends EXPR

  sealed trait Pattern {
    def isRest: Boolean = false
    def subpatterns: Seq[(SimplePattern, Seq[(PART[String], Option[Single])])]
    def position: Pos
  }

  sealed trait SimplePattern extends Pattern {
    def subpatterns: Seq[(SimplePattern, Seq[(PART[String], Option[Single])])] = Seq((this, Seq()))
  }

  sealed trait CompositePattern extends Pattern {
    def caseType: Option[Single]
    val patternsWithFields: Seq[(String, Pattern)]

    def subpatterns: Seq[(SimplePattern, Seq[(PART[String], Option[Single])])] =
      for {
        (field, p) <- patternsWithFields
        (sp, path) <- p.subpatterns
        nextPath = Expressions.PART.VALID(p.position, field)
      } yield (sp, path :+ ((nextPath, caseType)))
  }

  case class TypedVar(newVarName: Option[PART[String]], caseType: Type) extends SimplePattern {
    override def isRest: Boolean = caseType.isEmpty || caseType.isInstanceOf[AnyType]
    def position: Pos            = newVarName.fold(Pos.AnyPos: Pos)(_.position)
  }

  case class ConstsPat(constants: Seq[EXPR], position: Pos) extends SimplePattern

  case class TuplePat(patterns: Seq[Pattern], position: Pos) extends CompositePattern {
    val patternsWithFields: Seq[(String, Pattern)] = patterns.zipWithIndex.map { case (p, i) => (s"_${i + 1}", p) }
    val caseType: Option[Single]                   = None
  }

  case class ObjPat(patterns: Map[String, Pattern], objType: Single, position: Pos) extends CompositePattern {
    val patternsWithFields: Seq[(String, Pattern)] = patterns.toSeq
    val caseType: Option[Single]                   = Some(objType)
  }

  case class MATCH_CASE(
      position: Pos,
      pattern: Pattern,
      expr: EXPR,
      resultType: Option[FINAL] = None,
      ctxOpt: CtxOpt = None
  )

  object MATCH_CASE {
    def apply(
        position: Pos,
        newVarName: Option[PART[String]],
        types: Seq[PART[String]],
        expr: EXPR
    ): MATCH_CASE =
      MATCH_CASE(position, TypedVar(newVarName, Union(types.map(Single(_, None)))), expr)
  }

  case class MATCH(position: Pos, expr: EXPR, cases: Seq[MATCH_CASE], resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None) extends EXPR

  case class INVALID(position: Pos, message: String, resultType: Option[FINAL] = None, ctxOpt: CtxOpt = None) extends EXPR

  case class DAPP(position: Pos, decs: List[Declaration], fs: List[ANNOTATEDFUNC])

  case class SCRIPT(position: Pos, expr: EXPR)

  implicit class PartOps[T](val self: PART[T]) extends AnyVal {
    def toEither: Either[String, T] = self match {
      case Expressions.PART.VALID(_, x)         => Right(x)
      case Expressions.PART.INVALID(p, message) => Left(s"Can't compile an invalid instruction: $message in ${p.start}-${p.end}")
    }
  }

}
