package com.wavesplatform.lang.v1.compiler.compaction

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Terms._

import scala.annotation.tailrec
import scala.collection.Seq

trait ContractScriptCompactor {
  def compact(dApp: DApp): DApp
  def decompact(dApp: DApp): DApp

  def removeUnusedCode(dApp: DApp): DApp = {

    def getUsedNames(expr: EXPR): Seq[String] = {
      expr match {
        case BLOCK(dec, body)       => getUsedNames(dec.asInstanceOf[LET].value) ++ getUsedNames(body)
        case LET_BLOCK(dec, body)   => getUsedNames(dec.value) ++ getUsedNames(body)
        case FUNCTION_CALL(f, args) => f.funcName +: args.flatMap(getUsedNames)
        case GETTER(gExpr, _)       => getUsedNames(gExpr)
        case IF(cond, ifT, ifF)     => getUsedNames(cond) ++ getUsedNames(ifT) ++ getUsedNames(ifF)
        case REF(key)               => List(key)
        case _                      => List.empty
      }
    }

    @tailrec
    def getUsedNamesFromList(exprList: Seq[EXPR], prevNamesList: Seq[String]): Seq[String] = {
      val nextNameList = exprList.flatMap(getUsedNames).diff(prevNamesList)
      if (nextNameList.nonEmpty) {
        val nextExprList: Seq[EXPR] = dApp.decs.collect {
          case FUNC(name, _, body) if nextNameList.contains(name) => body
          case LET(name, value) if nextNameList.contains(name)    => value
        }
        getUsedNamesFromList(nextExprList, prevNamesList ++ nextNameList)
      } else {
        prevNamesList
      }
    }

    val usedNames = getUsedNamesFromList(
      dApp.callableFuncs.map(_.u.body) ++ dApp.verifierFuncOpt.map(f => List(f.u.body)).getOrElse(List.empty),
      Seq.empty
    )

    dApp.copy(decs = dApp.decs.filter(dec => usedNames.contains(dec.name)))
  }
}

object ContractScriptCompactor {
  val CharRange: scala.IndexedSeq[Char] = ('a' to 'z') ++ ('A' to 'Z')
}
