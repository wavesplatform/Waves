package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler

class MinerWithAccountScriptTestSuite extends BaseFunSuite {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.preactivatedFeatures(Seq(BlockchainFeatures.RideV6.id.toInt -> 0)*))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  test("scripted account are allowed to mine blocks after RideV6 feature activation") {
    val setScript = notMiner.setScript(miner.keyPair, Some(TestCompiler(V6).compileExpression("true").bytes().base64))
    nodes.waitForHeightAriseAndTxPresent(setScript.id)
    val transfer       = notMiner.transfer(notMiner.keyPair, miner.address, 1)
    val transferTxInfo = nodes.waitForTransaction(transfer.id)
    nodes.waitForHeightArise()
    notMiner.blockAt(transferTxInfo.height).generator shouldBe miner.address
    miner.blockAt(transferTxInfo.height).generator shouldBe miner.address
  }
}
