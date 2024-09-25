import java.io.File
import java.nio.file.{Files, Paths}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}
import org.hjson.JsonValue
import sbt.Keys.{baseDirectory, sourceManaged}
import sbt.{Def, IO, _}

import scala.collection.JavaConverters._

object Tasks {
  lazy val docSource = Def.task {
    val mapper = new ObjectMapper() with ClassTagExtensions
    mapper.registerModule(DefaultScalaModule)

    val baseLangDir = baseDirectory.value.getParentFile.getAbsolutePath

    def toMapChecked[K, V](data: Seq[V], key: V => K): Map[K, V] =
      data.distinct
        .groupBy(key)
        .ensuring(
          _.forall { case (_, v) =>
            if (v.size == 1) true
            else {
              println(v)
              false
            }
          },
          "Duplicate detected"
        )
        .mapValues(_.head)

    def str(s: String): String = "\"" + s + "\""

    def tupleStr(l: Seq[String]): String = l.mkString("(", ", ", ")")

    def listStr(l: Seq[String]): String = "List" + tupleStr(l)

    def mapStr(kv: Map[Seq[String], Seq[String]]): String = {
      val inner = kv
        .map { case (k, v) => Seq(tupleStr(k), tupleStr(v)) }
        .map(tupleStr)
      "Map" + tupleStr(inner.toSeq)
    }

    def sumMapStr(m1: String, m2: String): String = s"$m1 ++ $m2"

    def kvStr[K, V](
        seq: Seq[V],
        key: V => K,
        keyStr: V => Seq[String],
        valueStr: V => Seq[String]
    ): String =
      mapStr(
        toMapChecked(seq, key).map { case (_, v) => (keyStr(v), valueStr(v)) }
      )

    def buildVarsStr(vars: Seq[VarSourceData], version: Int): String =
      kvStr[String, VarSourceData](
        vars,
        _.name,
        v => Seq(str(v.name), version.toString),
        v => Seq(str(v.doc))
      )

    def buildFuncsStr(funcs: Seq[FuncSourceData], version: Int): String =
      kvStr[(String, List[String]), FuncSourceData](
        funcs,
        f => (f.name, f.params),
        f => Seq(str(f.name), listStr(f.params.map(str)), version.toString),
        f => Seq(str(f.doc), listStr(f.paramsDoc.map(str)), f.complexity.toString)
      )

    def buildTypesStr(vars: Seq[TypeSourceData], ver: String): String =
      kvStr[String, TypeSourceData](
        vars,
        _.name,
        v => Seq(str(v.name), ver),
        v => Seq(listStr(v.fields.map(f => tupleStr(Seq(str(f.name), str(f.`type`))))))
      )

    def readV1V2Data(): (String, String) =
      Seq(1, 2)
        .map { version =>
          val DocSourceData(vars, funcs) = mapper.readValue[DocSourceData](new File(s"$baseLangDir/doc/v$version/data.json"))
          val varDataStr                 = buildVarsStr(vars, version)
          val funcDataStr                = buildFuncsStr(funcs, version)
          (varDataStr, funcDataStr)
        }
        .reduce { (a, b) =>
          val (v1, f1) = a
          val (v2, f2) = b
          (
            sumMapStr(v1, v2),
            sumMapStr(f1, f2)
          )
        }

    def buildCategorizedFuncsStr(funcs: Seq[(FuncSourceData, String)], version: Int): String =
      kvStr[(String, List[String]), (FuncSourceData, String)](
        funcs,
        f => (f._1.name, f._1.params),
        f => Seq(str(f._1.name), listStr(f._1.params.map(str)), version.toString),
        f =>
          Seq(
            str(f._1.doc.replace("\n", "\\n")),
            listStr(f._1.paramsDoc.map(str).map(_.replace("\n", "\\n"))),
            str(f._2),
            f._1.complexity.toString
          )
      )

    def readFuncs(version: Int): String = {
      val funcs = for {
        path <- Files.list(Paths.get(s"$baseLangDir/doc/v$version/funcs")).iterator.asScala
        json = JsonValue.readHjson(Files.newBufferedReader(path)).asObject().toString
        funcs <- mapper.readValue[Map[String, List[FuncSourceData]]](json).head._2
        category = path.getName(path.getNameCount - 1).toString.split('.').head
      } yield (funcs, category)

      buildCategorizedFuncsStr(funcs.toSeq, version)
    }

    def readVars(version: Int): String = {
      val vars = mapper.readValue[Map[String, List[VarSourceData]]](new File(s"$baseLangDir/doc/v$version/vars.json")).head._2
      buildVarsStr(vars, version)
    }

    def readTypeData(ver: String): String = {
      val typesJson = JsonValue.readHjson(Files.newBufferedReader(Paths.get(s"$baseLangDir/doc/v$ver/types.hjson"))).asObject().toString
      val types     = mapper.readValue[Map[String, List[TypeSourceData]]](typesJson).head._2
      buildTypesStr(types, ver)
    }

    val docFolderR = "^v(\\d+)$".r
    val currentRideVersion =
      new File(s"$baseLangDir/doc")
        .listFiles()
        .map(_.name)
        .collect { case docFolderR(version) => version.toInt }
        .max

    val (v1V2Vars, v1V2Funcs) = readV1V2Data()
    val fromV3FuncDefs        = (3 to currentRideVersion).map(v => s"lazy val funcsV$v = ${readFuncs(v)}").mkString("\n    ")
    val fromV3VarDefs         = (3 to currentRideVersion).map(v => s"lazy val varsV$v = ${readVars(v)}").mkString("\n    ")
    val fromV3Vars            = (3 to currentRideVersion).map(v => s"varsV$v").mkString(" ++ ")
    val fromV3Funcs           = (3 to currentRideVersion).map(v => s"funcsV$v").mkString(" ++ ")
    val types                 = (1 to currentRideVersion).map(v => readTypeData(v.toString)).mkString(" ++ ")

    val sourceStr =
      s"""
         | package com.wavesplatform
         |
         | object DocSource {
         |   private val regex = "\\\\[(.+?)\\\\]\\\\(.+?\\\\)".r
         |
         |   $fromV3FuncDefs
         |   $fromV3VarDefs
         |
         |   lazy val typeData = $types
         |   lazy val varData  = $v1V2Vars ++ $fromV3Vars
         |   lazy val funcData = $v1V2Funcs ++ ($fromV3Funcs).view.mapValues(v => (regex.replaceAllIn(v._1, _.group(1)), v._2, v._4))
         | }
      """.stripMargin

    val rawDocFile = (Compile / sourceManaged).value / "com" / "wavesplatform" / "DocSource.scala"

    IO.write(rawDocFile, sourceStr)
    Seq(rawDocFile)
  }
}
