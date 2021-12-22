package com.wavesplatform.lang.v1.compiler

import scala.annotation.tailrec
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._

import scala.collection._
import scala.collection.mutable.ListBuffer

object ContractScriptCompactorV2 {
  private[this] val CharRange: scala.IndexedSeq[Char] = ('a' to 'z') ++ ('A' to 'Z')

  def compact(dApp: DApp,
              names: Seq[String] = Seq.empty,
              saveNameMapToMeta: Boolean = true): DApp = {

    var counter       = 0
    val originalNames = names.to(ListBuffer)

    @tailrec
    def idxToName(n: Int, seed: String = ""): String = {
      if (n < CharRange.length) String.valueOf(CharRange(n)) + seed
      else idxToName(n / CharRange.length - 1, String.valueOf(CharRange(n % CharRange.length)) + seed)
    }

    @tailrec
    def nameToIdx(name: String, acc: Int = 0): Int = {
      name.headOption match {
        case Some(v) => nameToIdx(name.drop(1), acc * CharRange.length + CharRange.indexOf(v) + 1)
        case None => acc - 1
      }
    }

    def createCompName(oldName: String): String = {
      if (saveNameMapToMeta) {
        val compNameIdx = originalNames.indexOf(oldName)
        if (compNameIdx != -1) {
          idxToName(compNameIdx)
        } else {
          def hasConflict(compactName: String) =
            dApp.callableFuncs.exists(_.u.name == compactName)

          var compactName = idxToName(counter)
          while (hasConflict(compactName)) {
            originalNames.addOne("")
            counter += 1
            compactName = idxToName(counter)
          }

          assert(!hasConflict(compactName))
          counter += 1
          originalNames.addOne(oldName)
          compactName
        }
      } else {
        val originalName = originalNames.applyOrElse[Int, String](nameToIdx(oldName), _ => oldName)
        if (originalName.isEmpty) oldName else originalName
      }
    }

    def getReplacedName(oldName: String): String = {
      if (saveNameMapToMeta) {
        val compNameIdx = originalNames.indexOf(oldName)
        if (compNameIdx != -1) idxToName(compNameIdx) else oldName
      } else {
        val originalName = originalNames.applyOrElse[Int, String](nameToIdx(oldName), _ => oldName)
        if (originalName.isEmpty) oldName else originalName
      }
    }

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

    val compDecs = dApp.decs.map(compactDec)
    val compCallableFuncs = dApp.callableFuncs.map { cFunc =>
      cFunc.copy(
        annotation = cFunc.annotation.copy(invocationArgName = createCompName(cFunc.annotation.invocationArgName)),
        u = cFunc.u.copy(
          args = cFunc.u.args.map(createCompName),
          body = compactExpr(cFunc.u.body)
        )
      )
    }
    val comVerifierFuncOpt = dApp.verifierFuncOpt.map { vFunc =>
      vFunc.copy(
        annotation = vFunc.annotation.copy(invocationArgName = createCompName(vFunc.annotation.invocationArgName)),
        u = compactDec(vFunc.u).asInstanceOf[FUNC]
      )
    }

    val resultOriginalNames = if (saveNameMapToMeta) {
      originalNames.to(immutable.Seq)
    } else {
      immutable.Seq.empty
    }
    val metaWithOriginalNames = dApp.meta.withOriginalNames(resultOriginalNames)

    dApp.copy(
      meta = metaWithOriginalNames,
      decs = compDecs,
      callableFuncs = compCallableFuncs,
      verifierFuncOpt = comVerifierFuncOpt
    )
  }

  def decompact(dApp: DApp): DApp = {
    if (dApp.meta.originalNames.nonEmpty) {
      compact(dApp, dApp.meta.originalNames, false)
    } else {
      dApp
    }
  }

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