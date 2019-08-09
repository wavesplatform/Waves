package com.wavesplatform.utils.doc

import java.io.FileWriter
import java.nio.file.{Files, Paths}

import com.github.mustachejava.DefaultMustacheFactory
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.DocSource
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, Expression, V3}

import scala.collection.JavaConverters._

object DocExportV3 {
  def main(args: Array[String]): Unit = {
      val funcV3Template = if (args.length == 1) args(0) else "lang/func-doc.template.md"
      val path = Paths.get("target/funcs")
      Files.createDirectories(path)

      val ds = DirectiveSet(V3, Account, Expression).explicitGet()
      RideFullContext.build(ds)
        .functions
        .map(
          f => {
            val argTypes = f.signature.args.map(_._2.toString).toList
            val docKey = (f.name, argTypes, V3.value.asInstanceOf[Int])
            val (doc, paramsDoc, category) = DocSource.categorizedfuncData(docKey)
            val varDocs =
              (f.args, f.signature.args, paramsDoc)
                .zipped
                .toList
                .map { arg => VarDoc(arg._1, TypeDoc(arg._2._2), arg._3.replace("\n", "<br>")) }
                .asJava

            val cost = f.costByLibVersion(V3).toString
            val funcDoc = FuncDoc(f.name, TypeDoc(f.signature.result), doc.replace("\n", "<br>"), varDocs, cost)
            (funcDoc, category)
          }
        )
        .groupBy(_._2)
        .map { case (category, funcs) =>
          val indexedDocs = funcs
            .zipWithIndex
            .map { case ((func, _), index) => FuncDocV3(func, index + 1) }
            .toList
            .asJava
          val title = category.replace("-", " ").capitalize
          val writer = new FileWriter(path.resolve(category + ".md").toFile)
          val docs = CategorizedFuncsDoc(indexedDocs, title)
          (writer, docs)
        }
        .foreach { case (writer, docs) =>
          new DefaultMustacheFactory().compile(funcV3Template).execute(writer, docs)
          writer.close()
        }
    }
}
