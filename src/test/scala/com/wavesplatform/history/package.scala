package com.wavesplatform

import com.wavesplatform.settings.{BlockchainSettings, FeaturesSettings}
import com.wavesplatform.state2._
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{Transaction, TransactionParser}

package object history {
  val MinInMemoryDiffSize = 5
  val DefaultBlockchainSettings = BlockchainSettings(
    blockchainFile = None,
    stateFile = None,
    checkpointFile = None,
    addressSchemeCharacter = 'N',
    minimumInMemoryDiffSize = MinInMemoryDiffSize,
    functionalitySettings = TestFunctionalitySettings.Enabled,
    genesisSettings = null)

  val ApplyMinerFeeWithTransactionSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(enableMicroblocksAfterHeight = 0))

  val ApplyMinerFeeBeforeAllTransactionsSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(enableMicroblocksAfterHeight = Long.MaxValue))

  val EmptyFeaturesSettings = FeaturesSettings(autoActivate = false, autoShutdownOnUnsupportedFeature = false, List.empty)

  def domain(bs: BlockchainSettings, featuresSettings: FeaturesSettings): Domain = {
    val (history, _, stateReader, blockchainUpdater, _) = StorageFactory(bs, featuresSettings).get
    Domain(history, stateReader, blockchainUpdater)
  }

  val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))
  val generationSignature = ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction]): Block = customBuildBlockOfTxs(refTo, txs, defaultSigner, 1, 0L)

  def customBuildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction],
                            signer: PrivateKeyAccount, version: Byte, timestamp: Long): Block =
    Block.buildAndSign(
      version = version,
      timestamp = timestamp,
      reference = refTo,
      consensusData = NxtLikeConsensusBlockData(
        baseTarget = 1L,
        generationSignature = generationSignature),
      transactionData = txs,
      signer = signer).explicitGet()


  def customBuildMicroBlockOfTxs(totalRefTo: ByteStr, prevTotal: Block, txs: Seq[Transaction],
                                 signer: PrivateKeyAccount, version: Byte, ts: Long): (Block, MicroBlock) = {
    val newTotalBlock = customBuildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs, signer, version, ts)
    val nonSigned = MicroBlock.buildAndSign(
      generator = signer,
      transactionData = txs,
      prevResBlockSig = prevTotal.uniqueId,
      totalResBlockSig = newTotalBlock.uniqueId
    ).explicitGet()
    (newTotalBlock, nonSigned)
  }


  def buildMicroBlockOfTxs(totalRefTo: ByteStr, prevTotal: Block, txs: Seq[Transaction],
                           signer: PrivateKeyAccount): (Block, MicroBlock) = {
    val newTotalBlock = buildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs)
    val nonSigned = MicroBlock.buildAndSign(
      generator = signer,
      transactionData = txs,
      prevResBlockSig = prevTotal.uniqueId,
      totalResBlockSig = newTotalBlock.uniqueId
    ).explicitGet()
    (newTotalBlock, nonSigned)
  }

  def randomSig: ByteStr = TestBlock.randomOfLength(Block.BlockIdLength)

  def chainBlocks(txs: Seq[Seq[Transaction]]): Seq[Block] = {
    def chainBlocksR(refTo: ByteStr, txs: Seq[Seq[Transaction]]): Seq[Block] = txs match {
      case (x :: xs) =>
        val block = buildBlockOfTxs(refTo, x)
        block +: chainBlocksR(block.uniqueId, xs)
      case _ => Seq.empty
    }

    chainBlocksR(randomSig, txs)
  }

  def chainBaseAndMicro(totalRefTo: ByteStr, base: Transaction, micros: Seq[Seq[Transaction]]): (Block, Seq[MicroBlock]) =
    chainBaseAndMicro(totalRefTo, Seq(base), micros, defaultSigner, 1, 0L)

  def chainBaseAndMicro(totalRefTo: ByteStr, base: Seq[Transaction], micros: Seq[Seq[Transaction]],
                        signer: PrivateKeyAccount, version: Byte, timestamp: Long): (Block, Seq[MicroBlock]) = {
    val block = customBuildBlockOfTxs(totalRefTo, base, signer, version, timestamp)
    val microBlocks = micros.foldLeft((block, Seq.empty[MicroBlock])) { case ((lastTotal, allMicros), txs) =>
      val (newTotal, micro) = customBuildMicroBlockOfTxs(totalRefTo, lastTotal, txs, signer, version, timestamp)
      (newTotal, allMicros :+ micro)
    }._2
    (block, microBlocks)
  }

  def spoilSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))
}
