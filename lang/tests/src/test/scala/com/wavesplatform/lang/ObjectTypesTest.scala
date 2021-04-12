package com.wavesplatform.lang

import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ObjectTypesTest extends PropSpec with PropertyChecks with Matchers with NoShrink {
  private val directives: Iterable[DirectiveSet] =
    DirectiveDictionary[StdLibVersion].all
      .map(version => DirectiveSet(version, Account, Expression).explicitGet())

  private def getDocFields(typeName: String, version: StdLibVersion): List[(String, String)] =
    DocSource.typeData
      .getOrElse(
        (typeName, version.id),
        throw new Exception(s"Type $typeName not found for $version")
      )

  private def fieldsString(fields: List[(String, String)]): String =
    fields.map { case (name, t) => s"$name: $t" }.mkString("\n\n", "\n", "\n\n")

  property("all object types") {
    directives.foreach { ds =>
      lazyContexts(ds)
        .value()
        .types
        .toList
        .collect {
          case CASETYPEREF(name, fields, _) =>
            val codeFields = fields.map { case (name, t) => (name, t.toString) }
            val docFields  = getDocFields(name, ds.stdLibVersion)
            if (docFields != codeFields)
              throw new TestFailedException(
                s"For type '$name' in ${ds.stdLibVersion} " +
                  s"documented fields ${fieldsString(docFields)} " +
                  s"differ from " +
                  s"code fields ${fieldsString(codeFields)}",
                0
              )
        }
    }
  }
}
