package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.Block
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class RideBlockInfoSuite extends BaseTransactionSuite {
  val activationHeight = 4

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.BlockReward, 2),
          (BlockchainFeatures.BlockV5, activationHeight)
        )
      )
      .buildNonConflicting()

  private val dAppScriptV4 =
    """
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func blockInfo(h: Int) = {
      |    let block = match blockInfoByHeight(h) {
      |        case block: BlockInfo => block
      |        case _ => throw("can't find block")
      |    }
      |    [
      |        IntegerEntry("timestamp", block.timestamp),
      |        IntegerEntry("height", block.height),
      |        IntegerEntry("baseTarget", block.baseTarget),
      |        BinaryEntry("generationSignature", block.generationSignature),
      |        BinaryEntry("generator", block.generator.bytes),
      |        BinaryEntry("generatorPublicKey", block.generatorPublicKey)
      |    ]
      |}
      |
      |@Callable(i)
      |func blockInfoV5(h: Int) = {
      |    let block = match blockInfoByHeight(h) {
      |        case block: BlockInfo => block
      |        case _ => throw("can't find block")
      |    }
      |    [
      |        IntegerEntry("timestamp", block.timestamp),
      |        IntegerEntry("height", block.height),
      |        IntegerEntry("baseTarget", block.baseTarget),
      |        BinaryEntry("generationSignature", block.generationSignature),
      |        BinaryEntry("generator", block.generator.bytes),
      |        BinaryEntry("generatorPublicKey", block.generatorPublicKey),
      |        BinaryEntry("vrf", block.vrf.value())
      |    ]
      |}
      |""".stripMargin

  private def caller = firstKeyPair
  private def dApp   = secondKeyPair

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    nodes.waitForHeight(activationHeight)
    val script = ScriptCompiler.compile(dAppScriptV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    miner.setScript(dApp, Some(script), waitForTx = true)
  }

  private lazy val dAppAddress: String = dApp.toAddress.toString

  test("able to retrieve block V5 info with vrf") {
    val height = activationHeight
    miner.invokeScript(caller, dAppAddress, func = Some("blockInfoV5"), args = List(CONST_LONG(height)), waitForTx = true)

    val block = miner.blockAt(height)
    checkCommonFields(block)
    miner.getDataByKey(dAppAddress, "vrf").value.asInstanceOf[ByteStr] shouldBe ByteStr.decodeBase58(block.vrf.get).get
  }

  test("not able to retrieve vrf from block V4") {
    assertBadRequestAndMessage(
      miner.invokeScript(caller, dAppAddress, func = Some("blockInfoV5"), args = List(CONST_LONG(activationHeight - 1))),
      "Error while executing account-script"
    )
  }

  test("not able to retrieve vrf from block V3") {
    assertBadRequestAndMessage(
      miner.invokeScript(caller, dAppAddress, func = Some("blockInfoV5"), args = List(CONST_LONG(activationHeight - 2))),
      "Error while executing account-script"
    )
  }

  test("able to retrieve block V4 info") {
    val height = activationHeight - 1
    miner.invokeScript(caller, dAppAddress, func = Some("blockInfo"), args = List(CONST_LONG(height)), waitForTx = true)

    checkCommonFields(miner.blockAt(height))
  }

  test("able to retrieve block V3 info") {
    val height = activationHeight - 2
    miner.invokeScript(caller, dAppAddress, func = Some("blockInfo"), args = List(CONST_LONG(height)), waitForTx = true)

    checkCommonFields(miner.blockAt(height))
  }

  test("able to retrieve genesis block info") {
    val height = 1
    miner.invokeScript(caller, dAppAddress, func = Some("blockInfo"), args = List(CONST_LONG(height)), waitForTx = true)

    checkCommonFields(miner.blockAt(height))
  }

  test("liquid blocks don't affect vrf field") {
    val height = miner.height + 1
    nodes.waitForHeight(height)

    miner.invokeScript(caller, dAppAddress, func = Some("blockInfoV5"), args = List(CONST_LONG(height)), waitForTx = true)
    val vrf1 = miner.getDataByKey(dAppAddress, "vrf").value.asInstanceOf[ByteStr]
    vrf1 shouldBe ByteStr.decodeBase58(miner.blockAt(height).vrf.get).get

    miner.transfer(caller, dAppAddress, 1, waitForTx = true)

    miner.invokeScript(caller, dAppAddress, func = Some("blockInfoV5"), args = List(CONST_LONG(height)), waitForTx = true)
    val vrf2 = miner.getDataByKey(dAppAddress, "vrf").value.asInstanceOf[ByteStr]
    vrf2 shouldBe ByteStr.decodeBase58(miner.blockAt(height).vrf.get).get

    vrf1 shouldBe vrf2
  }

  private def checkCommonFields(block: Block) = {
    miner.getDataByKey(dAppAddress, "timestamp").value.asInstanceOf[Long] shouldBe block.timestamp
    miner.getDataByKey(dAppAddress, "height").value.asInstanceOf[Long] shouldBe block.height
    miner.getDataByKey(dAppAddress, "baseTarget").value.asInstanceOf[Long] shouldBe block.baseTarget.get
    miner.getDataByKey(dAppAddress, "generationSignature").value.asInstanceOf[ByteStr].toString shouldBe block.generationSignature.get
    miner.getDataByKey(dAppAddress, "generator").value.asInstanceOf[ByteStr] shouldBe ByteStr.decodeBase58(block.generator).get
    miner.getDataByKey(dAppAddress, "generatorPublicKey").value.asInstanceOf[ByteStr].arr shouldBe block.generatorPublicKey.arr
  }
}
