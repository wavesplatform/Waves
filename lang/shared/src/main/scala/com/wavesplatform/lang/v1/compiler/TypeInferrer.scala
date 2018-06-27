package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.Types.UNION._
import com.wavesplatform.lang._

object TypeInferrer {

  case class MatchResult(tpe: TYPE, name: TYPEPLACEHOLDER.TYPEPARAM)
  // (ACTUAL, EXPECTED)
  def apply(seq: Seq[(TYPE, TYPEPLACEHOLDER)]): Either[String, Map[TYPEPLACEHOLDER.TYPEPARAM, TYPE]] = {
    val matching = seq.map(x => matchTypes(x._1, x._2))
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
              case p: PLAIN_TYPE => Right(p)
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

  def matchT(placeholder: TYPE, arg: TYPE): Boolean = {
    (placeholder, arg) match {
      case (_, NOTHING) => true
      case (NOTHING, _) => false
      case (p, a)       => UNION.create(p.l) >= UNION.create(a.l)
    }
  }

  def matchPlainCandidate(required: TYPE, actual: PLAIN_TYPE): Boolean =
    required match {
      case UNION(plaintypes) => plaintypes contains actual
      case p: PLAIN_TYPE     => p == actual
    }

  def matchTypes(argType: TYPE, placeholder: TYPEPLACEHOLDER): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $placeholder, actual: $argType"

    (placeholder, argType) match {
      case (placeholder: TYPE, _) =>
        Either.cond(matchPlainCandidate(placeholder, argType), None, err)
      case (tp @ TYPEPLACEHOLDER.TYPEPARAM(char), _) =>
        Right(Some(MatchResult(argType, tp)))
      case (tp @ TYPEPLACEHOLDER.LISTTYPEPARAM(innerTypeParam), LIST(t)) => matchTypes(t, innerTypeParam)
      case _                                                             => Left(err)
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
  def findCommonType(t1: TYPE, t2: TYPE): TYPE = {
    if (t2 == NOTHING) t1
    else if (t1 == NOTHING) t2
    else if (t1 == t2) t1
    else
      (t1, t2) match {
        case (r @ LIST(it1), a @ LIST(it2)) =>
          findCommonType(it1, it2) match {
            case UNION(_)      => UNION(r, a)
            case p: PLAIN_TYPE => LIST(p)
          }
        case (p1: PLAIN_TYPE, p2: PLAIN_TYPE) => if (p1 == p2) p1 else UNION(p1, p2)
        case (r: UNION, a: UNION)             => UNION.create((r.l.toSet ++ a.l.toSet).toSeq)
        case (r: UNION, t: PLAIN_TYPE)        => findCommonType(r, UNION(t))
        case (r: PLAIN_TYPE, t: UNION)        => findCommonType(UNION(r), t)
      }
  }
}
