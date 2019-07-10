package com.wavesplatform.doc

import java.io.File

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

class DocSource(filename: String) {
  private val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)

  private val (varData, funcData) = readDocData()

  private def readDocData(): (Map[String, VarSourceData], Map[(String, List[String]), FuncSourceData]) = {
    val DocSourceData(vars, funcs) = mapper.readValue[Map[String, DocSourceData]](new File(filename))
      .values
      .reduce((d1, d2) => DocSourceData(d1.vars ::: d2.vars, d1.funcs ::: d2.funcs))

    (bySingleName(vars), byNameAndParams(funcs))
  }

  private def bySingleName(vars: List[VarSourceData]): Map[String, VarSourceData] =
    vars
      .groupBy(_.name)
      .ensuring(_.forall { case (_, v) => v.size == 1 }, "Duplicate var detected")
      .mapValues(_.head)

  private def byNameAndParams(funcs: List[FuncSourceData]): Map[(String, List[String]), FuncSourceData] =
    funcs
      .distinct
      .groupBy(f => (f.name, f.params))
      .ensuring(_.forall { case (_, v) => if (v.size == 1) true else { println(v); false } }, "Duplicate func detected")
      .mapValues(_.head)

  def getVar(name: String): VarSourceData = varData(name)

  def getFunc(name: String, args: List[String]): FuncSourceData = funcData((name, args))
}
