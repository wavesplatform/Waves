package com.wavesplatform.lang.v1.compiler.compaction

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.compaction.ContractScriptCompactor._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.protobuf.dapp.DAppMeta.CompactNameAndOriginalNamePair

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object ContractScriptCompactorV1 extends ContractScriptCompactor {

  def compact(dApp: DApp): DApp =
    compact(dApp, immutable.Map[String, String](), saveNameMapToMeta = true)

  def decompact(dApp: DApp): DApp = {
    if (dApp.meta.compactNameAndOriginalNamePairList.nonEmpty) {
      val compactNameToOriginalNameMap = dApp.meta.compactNameAndOriginalNamePairList.map(pair => pair.compactName -> pair.originalName).toMap
      compact(dApp, compactNameToOriginalNameMap, saveNameMapToMeta = false)
    } else {
      dApp
    }
  }

  private def compact(dApp: DApp, nameMap: immutable.Map[String, String], saveNameMapToMeta: Boolean): DApp = {

    var counter                    = 0
    val originalToCompactedNameMap = nameMap.to(mutable.Map)

    def createCompName(oldName: String): String = {
      val compName = originalToCompactedNameMap.getOrElseUpdate(
        oldName, {
          def hasConflict(compactName: String) =
            dApp.callableFuncs.exists(_.u.name == compactName)

          @tailrec
          def generateName(n: Int, seed: String = ""): String = {
            if (n < CharRange.length) String.valueOf(CharRange(n)) + seed
            else generateName(n / CharRange.length - 1, String.valueOf(CharRange(n % CharRange.length)) + seed)
          }

          var compactName = generateName(counter)
          while (hasConflict(compactName)) {
            counter += 1
            compactName = generateName(counter)
          }

          assert(!hasConflict(compactName))
          counter += 1
          compactName
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

    val resultNamePairList = if (saveNameMapToMeta) {
      originalToCompactedNameMap.toSeq
        .sortBy(_._2) //sort by compactName
        .map {
          case (k, v) => CompactNameAndOriginalNamePair(v, k)
        }
    } else {
      immutable.Seq.empty[CompactNameAndOriginalNamePair]
    }
    val metaWithNameMap = dApp.meta.withCompactNameAndOriginalNamePairList(resultNamePairList)

    dApp.copy(
      meta = metaWithNameMap,
      decs = compDecs,
      callableFuncs = compCallableFuncs,
      verifierFuncOpt = comVerifierFuncOpt
    )
  }
}
