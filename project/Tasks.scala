import java.io.File

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import sbt.{Def, IO}
import sbt._
import sbt.Keys.{sourceManaged, version}

object Tasks {
  lazy val versionSource = (path: String) => Def.task {
    // WARNING!!!
    // Please, update the fallback version every major and minor releases.
    // This version is used then building from sources without Git repository
    // In case of not updating the version nodes build from headless sources will fail to connect to newer versions
    val FallbackVersion = (1, 0, 1)

    val versionFile      = sourceManaged.value / "com" / "wavesplatform" / "Version.scala"
    val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
    val (major, minor, patch) = version.value match {
      case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
      case _                            => FallbackVersion
    }
    IO.write(
      versionFile,
      s"""package $path
         |
         |object Version {
         |  val VersionString = "${version.value}"
         |  val VersionTuple = ($major, $minor, $patch)
         |}
         |""".stripMargin
    )
    Seq(versionFile)
  }

  lazy val docSource = Def.task {
    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)

    def readDocData(): (Map[String, VarSourceData], Map[(String, List[String]), FuncSourceData]) = {
      val DocSourceData(vars, funcs) = mapper.readValue[Map[String, DocSourceData]](new File("lang/doc-data.json"))
        .values
        .reduce((d1, d2) => DocSourceData(d1.vars ::: d2.vars, d1.funcs ::: d2.funcs))

      (
        toMapChecked(vars, (v: VarSourceData) => v.name),
        toMapChecked(funcs, (f: FuncSourceData) => (f.name, f.params))
      )
    }

    def toMapChecked[K, V](list: List[V], key: V => K): Map[K, V] =
      list
        .distinct
        .groupBy(key)
        .ensuring(_.forall { case (_, v) => if (v.size == 1) true else { println(v); false } }, "Duplicate detected")
        .mapValues(_.head)

    val (varData, funcData) = readDocData()

    val varDataStr = varData
      .map { case (k, v) => s"""	("$k", "${v.doc}")""" }
      .mkString("Map(\n", ",\n", "\n)")

    def listStr(l: List[String]) = l.map("\"" + _ + "\"").mkString("List(", ", ", ")")

    val funcDataStr = funcData
      .map { case (k, v) => s"""	(("${v.name}", ${listStr(v.params)}), ("${v.doc}", ${listStr(v.paramsDoc)}))""" }
      .mkString("Map(\n", ",\n", "\n)")

    val sourceStr =
      s"""
         | package com.wavesplatform
         |
         | object DocSource {
         |   val varData  = $varDataStr
         |   val funcData = $funcDataStr
         | }
     """.stripMargin

    val rawDocFile = sourceManaged.value / "com" / "wavesplatform" / "DocSource.scala"

    IO.write(rawDocFile, sourceStr)
    Seq(rawDocFile)
  }
}
