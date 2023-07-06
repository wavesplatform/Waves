package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.{BLOCK, DECLARATION, EXPR, FUNC, FUNCTION_CALL, GETTER, IF, LET, LET_BLOCK, REF}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames

import scala.annotation.tailrec

object ContractScriptCompactor {

  val CharRange: scala.IndexedSeq[Char] = ('a' to 'z') ++ ('A' to 'Z')

  type ReplaceNameF = (String, State) => CompactionResult[String]

  def compact(dApp: DApp): DApp =
    compact(dApp, State(0, Map.empty, Set.empty))

  def decompact(dApp: DApp): DApp =
    if (dApp.meta.originalNames.nonEmpty) {
      decompact(dApp, dApp.meta.originalNames.toVector)
    } else if (dApp.meta.compactNameAndOriginalNamePairList.nonEmpty) {
      val compactNameToOriginalNameMap = dApp.meta.compactNameAndOriginalNamePairList.map(pair => pair.compactName -> pair.originalName).toMap
      decompactOld(dApp, compactNameToOriginalNameMap)
    } else {
      dApp
    }

  def removeUnusedCode(dApp: DApp): DApp = {

    def getUsedNames(expr: EXPR): Seq[String] = {
      expr match {
        case BLOCK(let: LET, body)   => getUsedNames(let.value) ++ getUsedNames(body)
        case BLOCK(func: FUNC, body) => getUsedNames(func.body) ++ getUsedNames(body)
        case LET_BLOCK(dec, body)    => getUsedNames(dec.value) ++ getUsedNames(body)
        case FUNCTION_CALL(f, args)  => f.funcName +: args.flatMap(getUsedNames)
        case GETTER(gExpr, _)        => getUsedNames(gExpr)
        case IF(cond, ifT, ifF)      => getUsedNames(cond) ++ getUsedNames(ifT) ++ getUsedNames(ifF)
        case REF(key)                => List(key)
        case _                       => List.empty
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

  @tailrec
  private def createCompName(oldName: String, state: State, dApp: DApp): CompactionResult[String] =
    state.originalNames.get(oldName) match {
      case Some(compName) => CompactionResult(compName, state)
      case None =>
        val compactName = idxToName(state.counter)
        if (hasConflict(compactName, dApp)) {
          createCompName(oldName, State(state.counter + 1, state.originalNames, state.knownDecs), dApp)
        } else {
          assert(!hasConflict(compactName, dApp))
          CompactionResult(compactName, State(state.counter + 1, state.originalNames.updated(oldName, compactName), state.knownDecs))
        }
    }

  private def restoreOriginalName(oldName: String, state: State): CompactionResult[String] =
    CompactionResult(state.originalNames.getOrElse(oldName, oldName), state)

  protected def getReplacedName(oldName: String, state: State): String =
    state.originalNames.getOrElse(oldName, oldName)

  private def decompact(dApp: DApp, originalNames: Vector[String]): DApp = {
    val compNameToOriginalName = originalNames.zipWithIndex.map { case (originalName, idx) =>
      val compName           = idxToName(idx)
      val resultOriginalName = if (originalName.isEmpty) compName else originalName
      compName -> resultOriginalName
    }.toMap

    val compDAppRes = processDappWithoutMeta(dApp, State(0, compNameToOriginalName, Set.empty), restoreOriginalName)
    compDAppRes.value.copy(
      meta = dApp.meta.withOriginalNames(Vector.empty)
    )
  }

  private def decompactOld(dApp: DApp, originalNames: Map[String, String]): DApp = {
    val compDAppRes = processDappWithoutMeta(dApp, State(0, originalNames, Set.empty), restoreOriginalName)
    compDAppRes.value.copy(
      meta = dApp.meta.withCompactNameAndOriginalNamePairList(Seq.empty)
    )
  }

  private def compact(dApp: DApp, state: State): DApp = {
    val compDAppRes = processDappWithoutMeta(dApp, state, createCompName(_, _, dApp))
    val oldNameToIdx = compDAppRes.state.originalNames.map { case (oldName, compName) =>
      oldName -> nameToIdx(compName)
    }
    val emptyElemsWithIdx = oldNameToIdx.values.maxOption
      .map { maxIdx =>
        (0 to maxIdx).toSet
          .diff(oldNameToIdx.values.toSet)
          .map("" -> _)
      }
      .getOrElse(Set.empty)
    val originalNames = (oldNameToIdx.toVector ++ emptyElemsWithIdx).sortBy(_._2).map(_._1)
    compDAppRes.value.copy(
      meta = dApp.meta.withOriginalNames(originalNames)
    )
  }

  private def hasConflict(compactName: String, dApp: DApp): Boolean =
    dApp.callableFuncs.exists(_.u.name == compactName) || GlobalValNames.All.contains(compactName)

  private def processLet(let: LET, state: State, replaceNameF: ReplaceNameF): CompactionResult[LET] = {
    val compNameRes  = replaceNameF(let.name, state)
    val compValueRes = processExpr(let.value, compNameRes.state, replaceNameF)

    CompactionResult(
      let.copy(name = compNameRes.value, value = compValueRes.value),
      compValueRes.state.copy(knownDecs = compNameRes.state.knownDecs + let.name)
    )
  }

  private def processFunc(func: FUNC, state: State, replaceNameF: ReplaceNameF): CompactionResult[FUNC] = {
    val compNameRes = replaceNameF(func.name, state)
    val compArgsRes = processList(func.args, compNameRes.state, replaceNameF)
    val compBodyRes = processExpr(func.body, compArgsRes.state.addKnownDecs(func.args), replaceNameF)

    CompactionResult(
      func.copy(name = compNameRes.value, args = compArgsRes.value, body = compBodyRes.value),
      compBodyRes.state.copy(knownDecs = state.knownDecs + func.name)
    )
  }

  private def processDec(dec: DECLARATION, state: State, replaceNameF: ReplaceNameF): CompactionResult[DECLARATION] = {
    dec match {
      case l: LET  => processLet(l, state, replaceNameF)
      case f: FUNC => processFunc(f, state, replaceNameF)
      case other   => CompactionResult(other, state)
    }
  }

  private def processExpr(expr: EXPR, state: State, replaceNameF: ReplaceNameF): CompactionResult[EXPR] = {
    expr match {
      case b: BLOCK =>
        val compDecRes  = processDec(b.dec, state, replaceNameF)
        val compBodyRes = processExpr(b.body, compDecRes.state, replaceNameF)

        CompactionResult(b.copy(dec = compDecRes.value, body = compBodyRes.value), compBodyRes.state.copy(knownDecs = state.knownDecs))
      case lb: LET_BLOCK =>
        val compLetRes  = processLet(lb.let, state, replaceNameF)
        val compBodyRes = processExpr(lb.body, compLetRes.state, replaceNameF)

        CompactionResult(lb.copy(let = compLetRes.value, body = compBodyRes.value), compBodyRes.state.copy(knownDecs = state.knownDecs))
      case fc: FUNCTION_CALL =>
        val newFunction = fc.function match {
          case User(internalName, _) if state.knownDecs.contains(internalName) =>
            User(getReplacedName(internalName, state))
          case uF: User   => uF
          case nF: Native => nF
        }
        val compArgsRes = processList[EXPR](fc.args, state, processExpr(_, _, replaceNameF))

        CompactionResult(fc.copy(function = newFunction, args = compArgsRes.value), compArgsRes.state)
      case r: REF =>
        val newKey = if (state.knownDecs.contains(r.key)) {
          getReplacedName(r.key, state)
        } else {
          r.key
        }

        CompactionResult(r.copy(key = newKey), state)
      case g: GETTER =>
        val compExprRes = processExpr(g.expr, state, replaceNameF)

        CompactionResult(g.copy(expr = compExprRes.value), compExprRes.state)
      case iff: IF =>
        val compCondRes    = processExpr(iff.cond, state, replaceNameF)
        val compIfTrueRes  = processExpr(iff.ifTrue, compCondRes.state, replaceNameF)
        val compIfFalseRes = processExpr(iff.ifFalse, compIfTrueRes.state, replaceNameF)

        CompactionResult(iff.copy(cond = compCondRes.value, ifTrue = compIfTrueRes.value, ifFalse = compIfFalseRes.value), compIfFalseRes.state)
      case other =>
        CompactionResult(other, state)
    }
  }

  private def processDappWithoutMeta(dApp: DApp, state: State, replaceNameF: ReplaceNameF): CompactionResult[DApp] = {
    val compDecsRes = processList[DECLARATION](dApp.decs, state, processDec(_, _, replaceNameF))
    val compCallableFuncsRes = dApp.callableFuncs.foldLeft(CompactionResult(Vector.empty[DApp.CallableFunction], compDecsRes.state)) {
      case (CompactionResult(compFuncs, state), func) =>
        val compInvArgNameRes = replaceNameF(func.annotation.invocationArgName, state)
        val compArgsRes       = processList(func.u.args, compInvArgNameRes.state, replaceNameF)
        val compBodyRes = processExpr(func.u.body, compArgsRes.state.addKnownDecs(func.annotation.invocationArgName +: func.u.args), replaceNameF)
        val compFunc = func.copy(
          annotation = func.annotation.copy(invocationArgName = compInvArgNameRes.value),
          u = func.u.copy(args = compArgsRes.value, body = compBodyRes.value)
        )

        CompactionResult(compFuncs :+ compFunc, compBodyRes.state.copy(knownDecs = state.knownDecs))
    }

    val compVerifierFuncOptRes = dApp.verifierFuncOpt
      .fold[CompactionResult[Option[DApp.VerifierFunction]]](CompactionResult(None, compCallableFuncsRes.state)) { vFunc =>
        val compInvArgNameRes = replaceNameF(vFunc.annotation.invocationArgName, compCallableFuncsRes.state)
        val compFuncRes       = processFunc(vFunc.u, compInvArgNameRes.state.addKnownDecs(Seq(vFunc.annotation.invocationArgName)), replaceNameF)
        val newVFunc = vFunc.copy(
          annotation = vFunc.annotation.copy(invocationArgName = compInvArgNameRes.value),
          u = compFuncRes.value
        )
        CompactionResult(Some(newVFunc), compFuncRes.state.copy(knownDecs = compInvArgNameRes.state.knownDecs))
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
  private final def idxToName(n: Int, seed: String = ""): String = {
    if (n < CharRange.length) String.valueOf(CharRange(n)) + seed
    else idxToName(n / CharRange.length - 1, String.valueOf(CharRange(n % CharRange.length)) + seed)
  }

  @tailrec
  private final def nameToIdx(name: String, acc: Int = 0): Int = {
    name.headOption match {
      case Some(v) => nameToIdx(name.drop(1), acc * CharRange.length + CharRange.indexOf(v) + 1)
      case None    => acc - 1
    }
  }

  private def processList[A](list: List[A], state: State, compF: (A, State) => CompactionResult[A]): CompactionResult[List[A]] = {
    @tailrec
    def loop(list: List[A], compF: (A, State) => CompactionResult[A], acc: CompactionResult[Vector[A]]): CompactionResult[List[A]] = {
      list match {
        case Nil => acc.copy(value = acc.value.toList)
        case head :: tail =>
          val compArgRes = compF(head, acc.state)
          loop(tail, compF, CompactionResult(acc.value :+ compArgRes.value, compArgRes.state))
      }
    }

    loop(list, compF, CompactionResult(Vector.empty[A], state))
  }

  case class State(counter: Int, originalNames: Map[String, String], knownDecs: Set[String]) {
    def addKnownDecs(dec: Seq[String]): State = copy(knownDecs = knownDecs ++ dec)
  }
  case class CompactionResult[+A](value: A, state: State)
}
