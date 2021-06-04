import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, ScalaObjectMapper}
import org.hjson.JsonValue
import sbt.Keys.{baseDirectory, sourceManaged}
import sbt.{Def, IO, _}

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Tasks {
  lazy val docSource = Def.task {
    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)

    val baseLangDir = baseDirectory.value.getParentFile.getAbsolutePath

    def toMapChecked[K, V](data: Seq[V], key: V => K): Map[K, V] =
      data.distinct
        .groupBy(key)
        .ensuring(_.forall { case (_, v) => if (v.size == 1) true else { println(v); false } }, "Duplicate detected")
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

    def buildVarsStr(vars: Seq[VarSourceData], ver: String): String =
      kvStr[String, VarSourceData](
        vars,
        _.name,
        v => Seq(str(v.name), ver),
        v => Seq(str(v.doc))
      )

    def buildFuncsStr(funcs: Seq[FuncSourceData], ver: String): String =
      kvStr[(String, List[String]), FuncSourceData](
        funcs,
        f => (f.name, f.params),
        f => Seq(str(f.name), listStr(f.params.map(str)), ver),
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
      Seq("1", "2")
        .map { ver =>
          val DocSourceData(vars, funcs) = mapper.readValue[DocSourceData](new File(s"$baseLangDir/doc/v$ver/data.json"))
          val varDataStr                 = buildVarsStr(vars, ver)
          val funcDataStr                = buildFuncsStr(funcs, ver)
          (varDataStr, funcDataStr)
        }
        .reduce { (a, b) =>
          val (v1, f1) = a
          val (v2, f2) = b
          (
            sumMapStr(v1, v2),
            sumMapStr(f1, f2),
          )
        }

    def buildCategorizedFuncsStr(funcs: Seq[(FuncSourceData, String)], ver: String): String =
      kvStr[(String, List[String]), (FuncSourceData, String)](
        funcs,
        f => (f._1.name, f._1.params),
        f => Seq(str(f._1.name), listStr(f._1.params.map(str)), ver),
        f =>
          Seq(
            str(f._1.doc.replace("\n", "\\n")),
            listStr(f._1.paramsDoc.map(str).map(_.replace("\n", "\\n"))),
            str(f._2),
            f._1.complexity.toString
          )
      )

    def readCategorizedData(ver: String): (String, String) = {
      val funcs = for {
        path <- Files.list(Paths.get(s"$baseLangDir/doc/v$ver/funcs")).iterator.asScala
        json = JsonValue.readHjson(Files.newBufferedReader(path)).asObject().toString
        funcs <- mapper.readValue[Map[String, List[FuncSourceData]]](json).head._2
        category = path.getName(path.getNameCount - 1).toString.split('.').head
      } yield (funcs, category)

      val funcsStr = buildCategorizedFuncsStr(funcs.toSeq, ver)

      val vars    = mapper.readValue[Map[String, List[VarSourceData]]](new File(s"$baseLangDir/doc/v$ver/vars.json")).head._2
      val varsStr = buildVarsStr(vars, ver)

      (varsStr, funcsStr)
    }

    def readTypeData(ver: String): String = {
      val typesJson = JsonValue.readHjson(Files.newBufferedReader(Paths.get(s"$baseLangDir/doc/v$ver/types.hjson"))).asObject().toString
      val types     = mapper.readValue[Map[String, List[TypeSourceData]]](typesJson).head._2
      buildTypesStr(types, ver)
    }

    val types             = (1 to 5).map(v => readTypeData(v.toString)).mkString(" ++ ")
    val (vars, funcs)     = readV1V2Data()
    val (varsV3, funcsV3) = readCategorizedData("3")
    val (varsV4, funcsV4) = readCategorizedData("4")
    val (varsV5, funcsV5) = readCategorizedData("5")

    val sourceStr =
      s"""
         | package com.wavesplatform
         |
         | object DocSource {
         |   private val regex = "\\\\[(.+?)\\\\]\\\\(.+?\\\\)".r
         |
         |   lazy val typeData = $types
         |   lazy val varData  = $vars ++ $varsV3 ++ $varsV4 ++ $varsV5
         |   lazy val funcData = $funcs ++ (categorizedfuncDataV3 ++ categorizedfuncDataV4 ++ categorizedfuncDataV5).view.mapValues(v => (regex.replaceAllIn(v._1, _.group(1)), v._2, v._4))
         |   lazy val categorizedfuncDataV3 = $funcsV3
         |   lazy val categorizedfuncDataV4 = $funcsV4
         |   lazy val categorizedfuncDataV5 = $funcsV5
         | }
      """.stripMargin

    val rawDocFile = sourceManaged.value / "com" / "wavesplatform" / "DocSource.scala"

    IO.write(rawDocFile, sourceStr)
    Seq(rawDocFile)
  }
}
