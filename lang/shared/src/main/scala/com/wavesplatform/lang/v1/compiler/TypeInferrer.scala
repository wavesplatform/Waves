package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Types._

object TypeInferrer {

  case class MatchResult(tpe: FINAL, name: TYPEPARAM)
  def apply(seq: Seq[(FINAL, TYPE)], knownTypes: Map[String, FINAL] = Map.empty): Either[String, Map[TYPEPARAM, FINAL]] = {
    val matching = seq.map(x => matchTypes(x._1, x._2, knownTypes))
    matching.find(_.isLeft) match {
      case Some(left) => left.asInstanceOf[Left[String, Nothing]]
      case None =>
        val matchResults: Map[TYPEPARAM, Seq[MatchResult]] = matching.flatMap(_.explicitGet()).groupBy(_.name)
        val resolved = matchResults.mapValues {
          case h :: Nil => Right(h.tpe)
          case matchResults @ (h :: t) =>
            val commonType = t.map(_.tpe).toVector.foldLeft(h.tpe)(findCommonType)
            commonType match {
              case NOTHING   => Right(NOTHING)
              case p: SINGLE => Right(p)
              case u @ UNION(plainTypes, _) =>
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
                case UNION(x :: Nil, _) => x
                case x               => x
              }
            })
        }
    }
  }

  def matchTypes(argType: FINAL, placeholder: TYPE, knownTypes: Map[String, FINAL]): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $placeholder, actual: $argType"

    (placeholder, argType) match {
      case (tp @ TYPEPARAM(char), _) =>
        Right(Some(MatchResult(argType, tp)))
      case (tp @ PARAMETERIZEDLIST(innerTypeParam), LIST(t)) => matchTypes(t, innerTypeParam, knownTypes)
      case (tp @ PARAMETERIZEDLIST(_), _)                    => Left(err)
      case (tp @ PARAMETERIZEDUNION(l), _) =>
        val conctretes = UNION.create(
          l.filter(_.isInstanceOf[REAL])
            .map(_.asInstanceOf[REAL]))
        val parameterized = l
          .filter(_.isInstanceOf[PARAMETERIZED])
          .map(_.asInstanceOf[PARAMETERIZED])
        if (conctretes >= UNION.create(argType.typeList)) Right(None)
        else
          parameterized match {
            case singlePlaceholder :: Nil =>
              val nonMatchedArgTypes = argType match {
                case NOTHING         => ???
                case UNION(argTypes, _) => UNION(argTypes.filterNot(conctretes.typeList.contains))
                case s: SINGLE       => s
              }
              matchTypes(nonMatchedArgTypes, singlePlaceholder, knownTypes)
            case many => Left(s"Can't resolve correct type for parameterized $placeholder, actual: $argType")
          }

      case (LIST(tp), LIST(t)) => matchTypes(t, tp, knownTypes)
      case (placeholder: FINAL, _) =>
        Either.cond(placeholder >= UNION.create(argType.typeList), None, err)
    }
  }

  // match, e.g. many ifs
  def findCommonType(list: Seq[FINAL]): FINAL = list match {
    case one :: Nil => one
    case head :: tail =>
      val t = findCommonType(tail)
      findCommonType(head, t)
  }

  // if-then-else
  def findCommonType(t1: FINAL, t2: FINAL): FINAL = (t1, t2) match {
    case (t1, NOTHING)        => t1
    case (NOTHING, t2)        => t2
    case (t1, t2) if t1 == t2 => t1

    case (LIST(it1), LIST(it2)) => LIST(findCommonType(it1, it2))
    case (p1: SINGLE, p2: SINGLE) => UNION.create(List(p1, p2))
    case (r: UNION, a: UNION)     => UNION.create((r.typeList.toSet ++ a.typeList.toSet).toSeq)
    case (u: UNION, t: SINGLE)    => if (u.typeList.contains(t)) u else UNION.create(u.typeList :+ t)
    case (t: SINGLE, u: UNION)    => if (u.typeList.contains(t)) u else UNION.create(u.typeList :+ t)
  }
}
