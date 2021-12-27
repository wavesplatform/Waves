package com.wavesplatform.lang.v1.compiler.compaction

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.compaction.ContractScriptCompactor._

import scala.annotation.tailrec

object ContractScriptCompactorV2 extends ContractScriptCompactor[Vector[String]] {

  def compact(dApp: DApp): DApp =
    compact(dApp, State(0, Vector.empty), saveMeta = true)

  def decompact(dApp: DApp): DApp = {
    if (dApp.meta.originalNames.nonEmpty) {
      compact(dApp, State(0, dApp.meta.originalNames.toVector), saveMeta = false)
    } else {
      dApp
    }
  }

  @tailrec
  protected def createCompName(oldName: String, state: State[Vector[String]], dApp: DApp, saveMeta: Boolean): CompactionResult[String, Vector[String]] = {
    if (saveMeta) {
      val compNameIdx = state.originalNames.indexOf(oldName)
      if (compNameIdx != -1) {
        CompactionResult(idxToName(compNameIdx), state)
      } else {
        val compactName = idxToName(state.counter)
        if (hasConflict(compactName, dApp)) {
          createCompName(oldName, State(state.counter + 1, state.originalNames :+ ""), dApp, saveMeta)
        } else {
          assert(!hasConflict(compactName, dApp))
          CompactionResult(compactName, State(state.counter + 1, state.originalNames :+ oldName))
        }
      }
    } else {
      val originalName = state.originalNames.applyOrElse[Int, String](nameToIdx(oldName), _ => oldName)
      val name = if (originalName.isEmpty) oldName else originalName
      CompactionResult(name, state)
    }
  }

  protected def getReplacedName(oldName: String, state: State[Vector[String]], saveMeta: Boolean): String = {
    if (saveMeta) {
      val compNameIdx = state.originalNames.indexOf(oldName)
      if (compNameIdx != -1) idxToName(compNameIdx) else oldName
    } else {
      val originalName = state.originalNames.applyOrElse[Int, String](nameToIdx(oldName), _ => oldName)
      if (originalName.isEmpty) oldName else originalName
    }
  }

  private def compact(dApp: DApp, state: State[Vector[String]], saveMeta: Boolean): DApp = {

    val compDAppRes = compactDappWithoutMeta(dApp, state, saveMeta)

    val resultOriginalNames = if (saveMeta) {
      compDAppRes.state.originalNames
    } else {
      Vector.empty
    }
    val metaWithOriginalNames = dApp.meta.withOriginalNames(resultOriginalNames)

    compDAppRes.value.copy(
      meta = metaWithOriginalNames
    )
  }
}
