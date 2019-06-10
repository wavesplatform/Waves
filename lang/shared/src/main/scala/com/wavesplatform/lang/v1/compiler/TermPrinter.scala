package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CaseObj, EVALUATED}

object TermPrinter {
  def string(e: EVALUATED): String = {
    val sb = new StringBuilder()
    print(s => sb.append(s), e)
    sb.mkString
  }

  def indentObjString(e: CaseObj, depth: Int): String = {
    val sb = new StringBuilder()
    printObj(s => sb.append(s), e, depth)
    sb.mkString
  }

  def print(toDest: String => Unit, e: EVALUATED, depth: Int = 0): Unit =
    e match {
      case obj: CaseObj => printObj(toDest, obj, depth)
      case arr: ARR     => printArr(toDest, arr)
      case a            => toDest(a.prettyString(depth))
    }

  private def printArr(toDest: String => Unit, arr: ARR): Unit = {
    toDest("[")
    val length = arr.xs.length
    if (length > 0) {
      var i = 0
      while (i < length - 1) {
        print(toDest, arr.xs(i))
        toDest(", ")
        i = i + 1
      }
      print(toDest, arr.xs(length - 1))
    }
    toDest("]")
  }

  private def printObj(toDest: String => Unit, obj: CaseObj, depth: Int): Unit = {
    toDest(obj.caseType.name)
    if (obj.fields.nonEmpty) {
      toDest("(\n")
      obj.fields
        .foreach { case (name, value) =>
          indent(toDest, depth + 1)
          toDest(name)
          toDest(" = ")
          this.print(toDest, value, depth + 1)
          toDest("\n")
        }
      indent(toDest, depth)
      toDest(")")
    }
  }

  private def indent(toDest: String => Unit, n: Int): Unit = {
    var i = n
    while (i > 0) {
      toDest("\t")
      i = i - 1
    }
  }
}
