package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._

import scala.collection._

object ContractScriptCompactor {

  def compact(
      dApp: DApp,
      nameMap: immutable.Map[String, String] = immutable.Map[String, String](),
      saveNameMapToMeta: Boolean = true
  ): DApp = {

    val handledDApp = removeUnusedCode(dApp)

    val originalToCompactedNameMap: mutable.Map[String, String] = nameMap.to(mutable.Map)

    var currentNameCharIdx = 0
    var currentNameNum     = 1
    val charRange          = 'a' to 'z'
    def createCompName(oldName: String): String = {
      val compName = originalToCompactedNameMap.getOrElse(
        oldName,
        {
          val cName = s"${charRange(currentNameCharIdx)}${currentNameNum}"
          originalToCompactedNameMap += oldName -> cName
          currentNameNum += 1
          if (currentNameCharIdx < 25) {
            if (currentNameNum > 9) {
              currentNameCharIdx += 1
              currentNameNum = 1
            }
          }
          cName
        }
      )

      compName
    }

    def getReplacedName(oldName: String): String = originalToCompactedNameMap.getOrElse(oldName, oldName)

    def compactDec(dec: DECLARATION): DECLARATION = {
      dec match {
        case l: LET => l.copy(name = createCompName(l.name), value = compactExpr(l.value))
        case f: FUNC =>
          f.copy(
            name = createCompName(f.name),
            args = f.args.map(createCompName),
            body = compactExpr(f.body)
          )
        case other => other
      }
    }

    def compactExpr(expr: EXPR): EXPR = {
      expr match {
        case b: BLOCK =>
          b.copy(
            dec = compactDec(b.dec),
            body = compactExpr(b.body)
          )
        case lb: LET_BLOCK =>
          lb.copy(
            let = compactDec(lb.let).asInstanceOf[LET],
            body = compactExpr(lb.body)
          )
        case fc: FUNCTION_CALL =>
          fc.copy(
            function = fc.function match {
              case User(internalName, _) => User(getReplacedName(internalName))
              case nF: Native            => nF
            },
            args = fc.args.map(compactExpr)
          )
        case r: REF => r.copy(key = getReplacedName(r.key))
        case g: GETTER =>
          g.copy(
            expr = compactExpr(g.expr)
          )
        case iff: IF =>
          iff.copy(
            cond = compactExpr(iff.cond),
            ifTrue = compactExpr(iff.ifTrue),
            ifFalse = compactExpr(iff.ifFalse)
          )
        case other => other
      }
    }

    val compDecs = handledDApp.decs.map(compactDec)
    val compCallableFuncs = handledDApp.callableFuncs.map { cFunc =>
      cFunc.copy(
        annotation = cFunc.annotation.copy(invocationArgName = createCompName(cFunc.annotation.invocationArgName)),
        u = cFunc.u.copy(
          args = cFunc.u.args.map(createCompName),
          body = compactExpr(cFunc.u.body)
        )
      )
    }
    val comVerifierFuncOpt = handledDApp.verifierFuncOpt.map { vFunc =>
      vFunc.copy(
        annotation = vFunc.annotation.copy(invocationArgName = createCompName(vFunc.annotation.invocationArgName)),
        u = compactDec(vFunc.u).asInstanceOf[FUNC]
      )
    }

    val resultNameMap = if (saveNameMapToMeta) {
      originalToCompactedNameMap.map{ case (k, v) => v -> k }.toMap
    } else {
      immutable.HashMap[String, String]()
    }
    val metaWithNameMap = handledDApp.meta.withCompactNameToOriginalNameMap(resultNameMap)

    handledDApp.copy(
      meta = metaWithNameMap,
      decs = compDecs,
      callableFuncs = compCallableFuncs,
      verifierFuncOpt = comVerifierFuncOpt
    )
  }

  def decompact(dApp: DApp): DApp = {
    if (dApp.meta.compactNameToOriginalNameMap.nonEmpty) {
      compact(dApp, dApp.meta.compactNameToOriginalNameMap, false)
    } else {
      dApp
    }
  }

  private def removeUnusedCode(dApp: DApp): DApp = {

    def getUsedNames(expr: EXPR): Seq[String] = {
      expr match {
        case BLOCK(dec, body) => getUsedNames(dec.asInstanceOf[LET].value) ++ getUsedNames(body)
        case LET_BLOCK(dec, body) => getUsedNames(dec.value) ++ getUsedNames(body)
        case FUNCTION_CALL(f, args) => f.funcName +: args.flatMap(getUsedNames)
        case GETTER(gExpr, _) => getUsedNames(gExpr)
        case IF(cond, ifT, ifF) => getUsedNames(cond) ++ getUsedNames(ifT) ++ getUsedNames(ifF)
        case REF(key) => List(key)
        case _ => List.empty
      }
    }

    def getUsedNamesFromList(exprList: Seq[EXPR], prevNamesList: Seq[String]): Seq[String] = {
      val nextNameList = exprList.flatMap(getUsedNames).diff(prevNamesList)
      if (nextNameList.nonEmpty) {
        val nextExprList: Seq[EXPR] = dApp.decs.collect {
          case FUNC(name, _, body) if nextNameList.contains(name) => body
          case LET(name, value) if nextNameList.contains(name) => value
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
