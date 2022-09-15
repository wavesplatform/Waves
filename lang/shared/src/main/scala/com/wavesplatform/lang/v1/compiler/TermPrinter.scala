package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BYTESTR, CONST_STRING, CaseObj, EVALUATED}

case class TermPrinter(fixArrIndentation: Boolean = false) {
  def prettyString(e: EVALUATED, depth: Int): String = {
    e match {
      case obj: CaseObj => indentObjString(obj, depth)
      case arr: ARR     => indentArrString(arr, depth)
      case CONST_BYTESTR(bs) =>
        if (bs.size > 1024) {
          "base64'" ++ Base64.encode(bs.arr) ++ "'"
        } else {
          "base58'" ++ Base58.encode(bs.arr) ++ "'"
        }
      case CONST_STRING(s) =>
        "\"" ++ escape(s) ++ "\""
      case other => other.toString
    }
  }

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

  def indentArrString(e: ARR, depth: Int): String = {
    val sb = new StringBuilder()
    if (fixArrIndentation) printArr(s => sb.append(s), e, depth) else printArr(s => sb.append(s), e)
    sb.mkString
  }

  def print(toDest: String => Unit, e: EVALUATED, depth: Int = 0): Unit =
    e match {
      case obj: CaseObj => printObj(toDest, obj, depth)
      case arr: ARR     => if (fixArrIndentation) printArr(toDest, arr, depth) else printArr(toDest, arr)
      case a            => toDest(prettyString(a, depth))
    }

  private def printArr(toDest: String => Unit, arr: ARR, depth: Int): Unit = {
    toDest("[")
    val length = arr.xs.length
    if (length > 0) {
      var i = 0
      toDest("\n")
      while (i < length - 1) {
        indent(toDest, depth + 1)
        print(toDest, arr.xs(i), depth + 1)
        toDest(",\n")
        i = i + 1
      }
      indent(toDest, depth + 1)
      print(toDest, arr.xs(length - 1), depth + 1)
      toDest("\n")
      indent(toDest, depth)
      toDest("]")
    } else {
      toDest("]")
    }
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

  private def escape(s: String): String = {
    // Simple and very naive implementation based on
    // https://github.com/linkedin/dustjs/blob/3fc12efd153433a21fd79ac81e8c5f5d6f273a1c/dist/dust-core.js#L1099

    // Note this might not be the most efficient since Scala.js compiles this to a bunch of .split and .join calls
    s.replace("\\", "\\\\")
      .replace("/", "\\/")
      .replace("'", "\\'")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      .replace("\b", "\\b")
      .replace("\f", "\\f")
      .replace("\u2028", "\\u2028")
      .replace("\u2029", "\\u2029")
  }
}
