package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.Types.UNION._
import com.wavesplatform.lang._
import com.wavesplatform.lang.v1.evaluator.ctx.DefinedType

object TypeInferrer {

  case class MatchResult(tpe: TYPE, name: TYPEPLACEHOLDER.TYPEPARAM)
  // (ACTUAL, EXPECTED)
  def apply(seq: Seq[(TYPE, TYPEPLACEHOLDER)],
            knownTypes: Map[String, DefinedType] = Map.empty): Either[String, Map[TYPEPLACEHOLDER.TYPEPARAM, TYPE]] = {
    val matching = seq.map(x => matchTypes(x._1, x._2, knownTypes))
    matching.find(_.isLeft) match {
      case Some(left) => left.asInstanceOf[Left[String, Nothing]]
      case None =>
        val matchResults: Map[TYPEPLACEHOLDER.TYPEPARAM, Seq[MatchResult]] = matching.flatMap(_.explicitGet()).groupBy(_.name)

        // a function like Option[T], T => Option[Option[T]]
        // can lead to different interpretations of `T`.
        // `Nothing`, `TypeRef('XXX')` should find common type of `TypeRef('XXX')`

        val resolved = matchResults.mapValues {
          case h :: Nil => Right(h.tpe)
          case matchResults @ (h :: t) =>
            val commonType = t.map(_.tpe).toVector.foldLeft(h.tpe)(findCommonType)
            commonType match {
              case p: SINGLE_TYPE => Right(p)
              case u @ UNION(plainTypes) =>
                val commonTypeExists = plainTypes.exists { p =>
                  matchResults.map(_.tpe).forall(e => matchPlainCandidate(e, p))
                }
                Either.cond(commonTypeExists, u, s"Can't match inferred types of ${h.name} over ${matchResults.map(_.tpe)}")
            }
        }
        resolved.find(_._2.isLeft) match {
          case Some((_, left)) => left.asInstanceOf[Left[String, Nothing]]
          case None            => Right(resolved.mapValues(_.explicitGet()))
        }
    }
  }
  private def matchPlainCandidate(required: TYPE, actual: SINGLE_TYPE): Boolean =
    required match {
      case UNION(plaintypes) => plaintypes contains actual
      case p: SINGLE_TYPE    => p == actual
    }

  def matchTypes(argType: TYPE, placeholder: TYPEPLACEHOLDER, knownTypes: Map[String, DefinedType]): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $placeholder, actual: $argType"

    (placeholder, argType) match {
      case (tp @ TYPEPLACEHOLDER.TYPEPARAM(char), _) =>
        Right(Some(MatchResult(argType, tp)))
      case (tp @ TYPEPLACEHOLDER.LISTTYPEPARAM(innerTypeParam), LIST(t)) => matchTypes(t, innerTypeParam, knownTypes)
      case (tp @ TYPEPLACEHOLDER.LISTTYPEPARAM(_), _)                    => Left(err)
      case (placeholder: CONCRETE, _)                                    => Either.cond(Types.concreteToType(placeholder, knownTypes) >= UNION.create(argType.l), None, err)
    }
  }

  // match, e.g. many ifs
  def findCommonType(list: Seq[TYPE]): TYPE = list match {
    case one :: Nil => one
    case head :: tail =>
      val t = findCommonType(tail)
      findCommonType(head, t)
  }

  // if-then-else
  def findCommonType(t1: TYPE, t2: TYPE): TYPE = (t1, t2) match {
    case (t1, NOTHING)        => t1
    case (NOTHING, t2)        => t2
    case (t1, t2) if t1 == t2 => t1

    case (r @ LIST(it1), a @ LIST(it2)) =>
      findCommonType(it1, it2) match {
        case UNION(_)       => UNION(r, a)
        case p: SINGLE_TYPE => LIST(p)
      }
    case (p1: SINGLE_TYPE, p2: SINGLE_TYPE) => if (p1 == p2) p1 else UNION(p1, p2)
    case (r: UNION, a: UNION)               => UNION.create((r.l.toSet ++ a.l.toSet).toSeq)
    case (r: UNION, t: SINGLE_TYPE)         => findCommonType(r, UNION(t))
    case (r: SINGLE_TYPE, t: UNION)         => findCommonType(UNION(r), t)
  }
}
