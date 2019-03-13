package com.wavesplatform.lang

import cats.implicits._
import com.wavesplatform.lang.ScriptType.ScriptType
import com.wavesplatform.lang.ContentType.ContentType
import com.wavesplatform.lang.StdLibVersion.StdLibVersion
import com.wavesplatform.lang.directives.{Directive, DirectiveKey}

import scala.util.{Failure, Success, Try}

package object utils {

  def directiveConsistency(t: DirectiveSet): Either[String, DirectiveSet] = t match {
    case DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Contract) => Right(t)
    case DirectiveSet(_, _, ContentType.Expression)                               => Right(t)
    case _ =>
      Left(
        s"Inconsistent set of directives $t," +
          s" could be (V3, ACCOUNT, CONTRACT) or (<any>, <any>, EXPRESSION)")

  }

  def extractStdLibVersion(directives: List[Directive]): Either[String, StdLibVersion] = {
    directives
      .find(_.key == DirectiveKey.STDLIB_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            val ver = StdLibVersion(v)
            Either
              .cond(
                StdLibVersion.SupportedVersions(ver),
                ver,
                "Unsupported language version"
              )
          case Failure(ex) =>
            Left("Can't parse language version")
      })
      .getOrElse(StdLibVersion.V2.asRight)
  }

  def extractContentType(directives: List[Directive]): Either[String, ContentType] = {
    directives
      .find(_.key == DirectiveKey.CONTENT_TYPE)
      .map(d =>
        Try(d.value) match {
          case Success(v) =>
            val cType = ContentType.parseString(v)
            Either
              .cond(
                ContentType.SupportedTypes(cType),
                cType,
                "Unsupported content type"
              )
          case Failure(ex) =>
            Left("Can't parse content type")
      })
      .getOrElse(ContentType.Expression.asRight)
  }

  def extractScriptType(directives: List[Directive]): Either[String, ScriptType] = {
    directives
      .find(_.key == DirectiveKey.SCRIPT_TYPE)
      .map(d =>
        Try(d.value) match {
          case Success(v) =>
            val sType = ScriptType.parseString(v)
            Either
              .cond(
                ScriptType.SupportedTypes(sType),
                sType,
                "Unsupported script type"
              )
          case Failure(ex) =>
            Left("Can't parse script type")
      })
      .getOrElse(ScriptType.Account.asRight)
  }

  def extractDirectives(directives: List[Directive]): Either[String, DirectiveSet] =
    for {
      v <- extractStdLibVersion(directives)
      c <- extractContentType(directives)
      t <- extractScriptType(directives)
      r <- directiveConsistency(DirectiveSet(v, t, c))
    } yield r

}
