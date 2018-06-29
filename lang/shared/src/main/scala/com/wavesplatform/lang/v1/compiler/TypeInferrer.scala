package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Types._
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
              case NOTHING   => Right(NOTHING)
              case p: SINGLE => Right(p)
              case u @ UNION(plainTypes) =>
                val commonTypeExists = plainTypes.exists { p =>
                  matchResults.map(_.tpe).forall(e => e >= p)
                }
                Either.cond(commonTypeExists, u, s"Can't match inferred types of ${h.name} over ${matchResults.map(_.tpe)}")
            }
        }
        resolved.find(_._2.isLeft) match {
          case Some((_, left)) => left.asInstanceOf[Left[String, Nothing]]
          case None =>
            Right(resolved.mapValues { t =>
              t.explicitGet() match {
                case UNION(x :: Nil) => x
                case x               => x
              }
            })
        }
    }
  }

  def matchTypes(argType: TYPE, placeholder: TYPEPLACEHOLDER, knownTypes: Map[String, DefinedType]): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $placeholder, actual: $argType"

    (placeholder, argType) match {
      case (_, NOTHING) => Right(None)
      case (tp @ TYPEPLACEHOLDER.TYPEPARAM(char), _) =>
        Right(Some(MatchResult(argType, tp)))
      case (tp @ TYPEPLACEHOLDER.PARAMETERIZEDLIST(innerTypeParam), LIST(t)) => matchTypes(t, innerTypeParam, knownTypes)
      case (tp @ TYPEPLACEHOLDER.PARAMETERIZEDLIST(_), _)                    => Left(err)
      case (tp @ TYPEPLACEHOLDER.PARAMETERIZEDUNION(l), _) =>
        val conctretes = UNION.create(
          l.filter(_.isInstanceOf[FINAL])
            .map(_.asInstanceOf[FINAL]))
        val parameterized = l
          .filter(_.isInstanceOf[PARAMETERIZED])
          .map(_.asInstanceOf[PARAMETERIZED])
        if (conctretes >= UNION.create(argType.l)) Right(None)
        else
          parameterized match {
            case singlePlaceholder :: Nil =>
              val nonMatchedArgTypes = argType match {
                case NOTHING         => ???
                case UNION(argTypes) => UNION(argTypes.filterNot(conctretes.l.contains))
                case s: SINGLE       => s
              }
              matchTypes(nonMatchedArgTypes, singlePlaceholder, knownTypes)
            case many => Left(s"Can't resolve correct type for parameterized $placeholder, actual: $argType")
          }

      case (placeholder: FINAL, _) =>
        Either.cond(placeholder >= UNION.create(argType.l), None, err)
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
        case NOTHING   => NOTHING
        case UNION(_)  => UNION(List(r, a))
        case p: SINGLE => LIST(p)
      }
    case (p1: SINGLE, p2: SINGLE) => if (p1 == p2) p1 else UNION.create(List(p1, p2))
    case (r: UNION, a: UNION)     => UNION.create((r.l.toSet ++ a.l.toSet).toSeq)
    case (u: UNION, t: SINGLE)    => if (u.l.contains(t)) u else UNION.create(u.l :+ t)
    case (t: SINGLE, u: UNION)    => if (u.l.contains(t)) u else UNION.create(u.l :+ t)
  }
}
