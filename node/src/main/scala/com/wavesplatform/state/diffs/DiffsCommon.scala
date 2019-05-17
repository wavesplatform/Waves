package com.wavesplatform.state.diffs

import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.ProvenTransaction

private[diffs] object DiffsCommon {
  def countScriptRuns(blockchain: Blockchain, tx: ProvenTransaction): Int =
    tx.checkedAssets().count(blockchain.hasAssetScript) + Some(tx.sender.toAddress).count(blockchain.hasScript)

  def countScriptsComplexity(blockchain: Blockchain, tx: ProvenTransaction): Long = {
    val assetsComplexity = tx
      .checkedAssets()
      .flatMap(blockchain.assetDescription)
      .flatMap(_.script)
      .map(_.complexity)
      .sum

    val accountComplexity = blockchain
      .accountScript(tx.sender.toAddress)
      .fold(0L)(_.complexity)

    assetsComplexity + accountComplexity
  }
}
