package com.wavesplatform.lang.v1.compiler.compaction

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.compaction.ContractScriptCompactor._

import scala.annotation.tailrec
import scala.collection.Seq

trait ContractScriptCompactor[C <: Iterable[_]] {
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

  protected def createCompName(oldName: String, state: State[C], dApp: DApp, saveMeta: Boolean): CompactionResult[String, C]

  protected def getReplacedName(oldName: String, state: State[C], saveMeta: Boolean): String

  protected def hasConflict(compactName: String, dApp: DApp): Boolean =
    dApp.callableFuncs.exists(_.u.name == compactName)

  protected def compactDec(dec: DECLARATION, state: State[C], dApp: DApp, saveMeta: Boolean): CompactionResult[DECLARATION, C] = {
    dec match {
      case l: LET =>
        val compNameRes = createCompName(l.name, state, dApp, saveMeta)
        val compValueRes = compactExpr(l.value, compNameRes.state, dApp, saveMeta)

        CompactionResult(l.copy(name = compNameRes.value, value = compValueRes.value), compValueRes.state)
      case f: FUNC =>
        val compNameRes = createCompName(f.name, state, dApp, saveMeta)
        val compArgsRes = compactList(f.args, compNameRes.state, createCompName(_, _, dApp, saveMeta))
        val compBodyRes = compactExpr(f.body, compArgsRes.state, dApp, saveMeta)

        CompactionResult(f.copy(name = compNameRes.value, args = compArgsRes.value, body = compBodyRes.value), compBodyRes.state)
      case other => CompactionResult(other, state)
    }
  }

  protected def compactExpr(expr: EXPR, state: State[C], dApp: DApp, saveMeta: Boolean): CompactionResult[EXPR, C] = {
    expr match {
      case b: BLOCK =>
        val compDecRes = compactDec(b.dec, state, dApp, saveMeta)
        val compBodyRes = compactExpr(b.body, compDecRes.state, dApp, saveMeta)

        CompactionResult(b.copy(dec = compDecRes.value, body = compBodyRes.value), compBodyRes.state)
      case lb: LET_BLOCK =>
        val compLetRes = compactDec(lb.let, state, dApp, saveMeta)
        val compBodyRes = compactExpr(lb.body, compLetRes.state, dApp, saveMeta)

        CompactionResult(lb.copy(let = compLetRes.value.asInstanceOf[LET], body = compBodyRes.value), compBodyRes.state)
      case fc: FUNCTION_CALL =>
        val newFunction = fc.function match {
          case User(internalName, _) => User(getReplacedName(internalName, state, saveMeta))
          case nF: Native            => nF
        }
        val compArgsRes = compactList(fc.args, state, compactExpr(_, _, dApp, saveMeta))

        CompactionResult(fc.copy(function = newFunction, args = compArgsRes.value), compArgsRes.state)
      case r: REF =>
        CompactionResult(r.copy(key = getReplacedName(r.key, state, saveMeta)), state)
      case g: GETTER =>
        val compExprRes = compactExpr(g.expr, state, dApp, saveMeta)

        CompactionResult(g.copy(expr = compExprRes.value), compExprRes.state)
      case iff: IF =>
        val compCondRes = compactExpr(iff.cond, state, dApp, saveMeta)
        val compIfTrueRes = compactExpr(iff.ifTrue, compCondRes.state, dApp, saveMeta)
        val compIfFalseRes = compactExpr(iff.ifFalse, compIfTrueRes.state, dApp, saveMeta)

        CompactionResult(iff.copy(cond = compCondRes.value, ifTrue = compIfTrueRes.value, ifFalse = compIfFalseRes.value), compIfFalseRes.state)
      case other =>
        CompactionResult(other, state)
    }
  }

  protected def compactDappWithoutMeta(dApp: DApp, state: State[C], saveMeta: Boolean): CompactionResult[DApp, C] = {
    val compDecsRes = compactList(dApp.decs, state, compactDec(_, _, dApp, saveMeta))
    val compCallableFuncsRes = dApp.callableFuncs.foldLeft(CompactionResult(Vector.empty[DApp.CallableFunction], compDecsRes.state)) {
      case (CompactionResult(compFuncs, state), func) =>
        val compInvArgNameRes = createCompName(func.annotation.invocationArgName, state, dApp, saveMeta)
        val compArgsRes = compactList(func.u.args, compInvArgNameRes.state, createCompName(_, _, dApp, saveMeta))
        val compBodyRes = compactExpr(func.u.body, compArgsRes.state, dApp, saveMeta)
        val compFunc = func.copy(
          annotation = func.annotation.copy(invocationArgName = compInvArgNameRes.value),
          u = func.u.copy(args = compArgsRes.value, body = compBodyRes.value)
        )

        CompactionResult(compFuncs :+ compFunc, compBodyRes.state)
    }

    val compVerifierFuncOptRes = dApp.verifierFuncOpt
      .fold[CompactionResult[Option[DApp.VerifierFunction], C]](CompactionResult(None, compCallableFuncsRes.state)) { vFunc =>
        val compInvArgNameRes = createCompName(vFunc.annotation.invocationArgName, compCallableFuncsRes.state, dApp, saveMeta)
        val compFuncRes = compactDec(vFunc.u, compInvArgNameRes.state, dApp, saveMeta)
        val newVFunc = vFunc.copy(
          annotation = vFunc.annotation.copy(invocationArgName = compInvArgNameRes.value),
          u = compFuncRes.value.asInstanceOf[FUNC]
        )
        CompactionResult(Some(newVFunc), compFuncRes.state)
      }

    CompactionResult(
      value = dApp.copy(
        decs = compDecsRes.value,
        callableFuncs = compCallableFuncsRes.value.toList,
        verifierFuncOpt = compVerifierFuncOptRes.value
      ),
      state = compVerifierFuncOptRes.state
    )
  }

  @tailrec
  protected final def idxToName(n: Int, seed: String = ""): String = {
    if (n < CharRange.length) String.valueOf(CharRange(n)) + seed
    else idxToName(n / CharRange.length - 1, String.valueOf(CharRange(n % CharRange.length)) + seed)
  }

  @tailrec
  protected final def nameToIdx(name: String, acc: Int = 0): Int = {
    name.headOption match {
      case Some(v) => nameToIdx(name.drop(1), acc * CharRange.length + CharRange.indexOf(v) + 1)
      case None    => acc - 1
    }
  }

  private def compactList[A](list: List[A], state: State[C], compF: (A, State[C]) => CompactionResult[A, C]): CompactionResult[List[A], C] = {
    val result = list.foldLeft(CompactionResult(Vector.empty[A], state)) {
      case (CompactionResult(compList, state), elem) =>
        val compArgRes = compF(elem, state)
        CompactionResult(compList :+ compArgRes.value, compArgRes.state)
    }
    result.copy(value = result.value.toList)
  }
}

object ContractScriptCompactor {
  val CharRange: scala.IndexedSeq[Char] = ('a' to 'z') ++ ('A' to 'Z')

  case class State[C <: Iterable[_]](counter: Int, originalNames: C)
  case class CompactionResult[A, C <: Iterable[_]](value: A, state: State[C])
}
