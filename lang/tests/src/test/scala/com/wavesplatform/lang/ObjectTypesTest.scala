package com.wavesplatform.lang

import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
import com.wavesplatform.test._
import org.scalatest.exceptions.TestFailedException

class ObjectTypesTest extends PropSpec {
  private val directives: Iterable[DirectiveSet] =
    DirectiveDictionary[StdLibVersion].all
      .map(version => DirectiveSet(version, Account, Expression).explicitGet())

  private def fieldsString(fields: List[(String, String)]): String =
    fields.map { case (name, t) => s"$name: $t" }.mkString("\n\n", "\n", "\n\n")

  private def check(scalaTypes: Seq[FINAL], version: StdLibVersion): Unit = {
    val docTypes =
      DocSource.typeData.collect { case ((typeName, v), fields) if v == version.id => (typeName, fields) }.toMap
    val unusedDocTypes =
      scalaTypes
        .collect { case CASETYPEREF(typeName, fields, _) => typeName -> fields.map { case (fieldName, t) => (fieldName, t.toString) } }
        .foldLeft(docTypes) {
          case (remainingDocTypes, (scalaTypeName, scalaTypeFields)) =>
            val docFields =
              remainingDocTypes.getOrElse(
                scalaTypeName,
                throw new TestFailedException(s"Can't find type '$scalaTypeName' in $version documentation", 0)
              )
            if (docFields != scalaTypeFields)
              throw new TestFailedException(
                s"For type '$scalaTypeName' in $version " +
                  s"documented fields ${fieldsString(docFields)} " +
                  s"differ from " +
                  s"code fields ${fieldsString(scalaTypeFields)}",
                0
              )
            remainingDocTypes - scalaTypeName
        }
    if (unusedDocTypes.nonEmpty)
      throw new TestFailedException(
        s"For RIDE $version documented types is unused: ${unusedDocTypes.map(_._1).mkString(", ")}",
        0
      )
  }

  property("all object types") {
    directives.foreach(ds => check(lazyContexts(ds).value().types, ds.stdLibVersion))
  }
}
