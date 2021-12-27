package com.wavesplatform.lang.v1.compiler.compaction

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.compaction.ContractScriptCompactor._
import com.wavesplatform.protobuf.dapp.DAppMeta.CompactNameAndOriginalNamePair

import scala.annotation.tailrec


object ContractScriptCompactorV1 extends ContractScriptCompactor[Map[String, String]] {

  def compact(dApp: DApp): DApp =
    compact(dApp, State(0, Map.empty[String, String]), saveMeta = true)

  def decompact(dApp: DApp): DApp = {
    if (dApp.meta.compactNameAndOriginalNamePairList.nonEmpty) {
      val compactNameToOriginalNameMap = dApp.meta.compactNameAndOriginalNamePairList.map(pair => pair.compactName -> pair.originalName).toMap
      compact(dApp, State(0, compactNameToOriginalNameMap), saveMeta = false)
    } else {
      dApp
    }
  }

  @tailrec
  protected def createCompName(oldName: String, state: State[Map[String, String]], dApp: DApp, saveMeta: Boolean): CompactionResult[String, Map[String, String]] =
    state.originalNames.get(oldName) match {
      case Some(compName) => CompactionResult(compName, state)
      case None =>
        val compactName = idxToName(state.counter)
        if (hasConflict(compactName, dApp)) {
          createCompName(oldName, State(state.counter + 1, state.originalNames), dApp, saveMeta)
        } else {
          assert(!hasConflict(compactName, dApp))
          CompactionResult(compactName, State(state.counter + 1, state.originalNames.updated(oldName, compactName)))
        }
    }

  protected def getReplacedName(oldName: String, state: State[Map[String, String]], saveMeta: Boolean): String =
    state.originalNames.getOrElse(oldName, oldName)

  private def compact(dApp: DApp, state: State[Map[String, String]], saveMeta: Boolean): DApp = {

    val compDAppRes = compactDappWithoutMeta(dApp, state, saveMeta)

    val resultNamePairList = if (saveMeta) {
      compDAppRes.state.originalNames.toSeq
        .sortBy(_._2) //sort by compactName
        .map {
          case (k, v) => CompactNameAndOriginalNamePair(v, k)
        }
    } else {
      Seq.empty[CompactNameAndOriginalNamePair]
    }
    val metaWithNameMap = dApp.meta.withCompactNameAndOriginalNamePairList(resultNamePairList)

    compDAppRes.value.copy(
      meta = metaWithNameMap
    )
  }
}
