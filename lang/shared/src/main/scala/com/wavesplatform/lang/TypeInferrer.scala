package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._

object TypeInferrer {

  case class MatchResult(tpe: TYPE, name: TYPEPARAM)
  // (ACTUAL, EXPECTED)
  def apply(seq: Seq[(TYPE, TYPEPLACEHOLDER)]): Either[String, Map[TYPEPARAM, TYPE]] = {
    val matching = seq.map(x => matchTypes(x._1, x._2))
    matching.find(_.isLeft) match {
      case Some(left) => left.asInstanceOf[Left[String, Nothing]]
      case None =>
        import cats.data._
        import cats.syntax.all._
        import cats.instances.option._
        import cats.instances.vector._
        val matchResults = matching.flatMap(_.right.get).groupBy(_.name)

        // a function like Option[T], T => Option[Option[T]]
        // can lead to different interpretations of `T`.
        // `Nothing`, `TypeRef('XXX')` should find common type of `TypeRef('XXX')`
        val resolved = matchResults.mapValues {
          case tpe :: Nil          => Right(tpe.tpe)
          case headTpe :: restTpes =>
            val maybeCommonType = restTpes.map(_.tpe).toVector.foldLeftM(headTpe.tpe)(findCommonType)
            maybeCommonType match {
              case None => Left(s"Can't match types ${headTpe :: restTpes}")
              case Some(commonType) => Right(commonType)
            }
        }

        resolved.find(_._2.isLeft) match {
          case Some(left) => left.asInstanceOf[Left[String, Nothing]]
          case None => Right(resolved.mapValues(_.right.get))
        }
    }
  }

  def matchTypes(actual: TYPE, expected: TYPEPLACEHOLDER): Either[String, Option[MatchResult]] = {
    lazy val err = s"Non-matching types: expected: $expected, actual: $actual"

    expected match {
      case realType: TYPE =>
        Either.cond(matchType(realType, actual).isDefined, None, err)
      case tp @ TYPEPARAM(char) =>
        Right(Some(MatchResult(actual, tp)))
      case tp @ OPTIONTYPEPARAM(innerTypeParam) =>
        actual match {
          case OPTION(t) => matchTypes(t, innerTypeParam)
          case _         => Left(err)
        }
    }
  }
}
