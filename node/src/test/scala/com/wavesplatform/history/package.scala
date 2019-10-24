package com.wavesplatform

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings._
import com.wavesplatform.transaction.Transaction

package object history {
  val MaxTransactionsPerBlockDiff = 10
  val MaxBlocksInMemory           = 5
  val DefaultBaseTarget           = 1000L
  val DefaultBlockchainSettings = BlockchainSettings(
    addressSchemeCharacter = 'N',
    functionalitySettings = TestFunctionalitySettings.Enabled,
    genesisSettings = GenesisSettings.TESTNET,
    rewardsSettings = RewardsSettings.TESTNET
  )

  val config   = ConfigFactory.load()
  val settings = WavesSettings.fromRootConfig(config)

  val MicroblocksActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 0)))

  val DataAndMicroblocksActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)))

  val TransfersV2ActivatedAt0BlockchainSettings: BlockchainSettings =
    DefaultBlockchainSettings.copy(
      functionalitySettings =
        DefaultBlockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0)))

  val MicroblocksActivatedAt0WavesSettings: WavesSettings = settings.copy(blockchainSettings = MicroblocksActivatedAt0BlockchainSettings)

  val DataAndMicroblocksActivatedAt0WavesSettings: WavesSettings = settings.copy(blockchainSettings = DataAndMicroblocksActivatedAt0BlockchainSettings)

  val TransfersV2ActivatedAt0WavesSettings: WavesSettings = settings.copy(blockchainSettings = TransfersV2ActivatedAt0BlockchainSettings)

  val DefaultWavesSettings: WavesSettings = settings.copy(blockchainSettings = DefaultBlockchainSettings, featuresSettings = settings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))

  val defaultSigner       = KeyPair(Array.fill(KeyLength)(0: Byte))
  val generationSignature = ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction]): Block =
    buildBlockOfTxs(refTo, txs, txs.headOption.fold(0L)(_.timestamp))

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction], timestamp: Long): Block =
    customBuildBlockOfTxs(refTo, txs, defaultSigner, 1, timestamp)

  def customBuildBlockOfTxs(refTo: ByteStr,
                            txs: Seq[Transaction],
                            signer: KeyPair,
                            version: Byte,
                            timestamp: Long,
                            bTarget: Long = DefaultBaseTarget): Block =
    Block
      .buildAndSign(
        version = version,
        timestamp = timestamp,
        reference = refTo,
        consensus = NxtLikeConsensusBlockData(baseTarget = bTarget, generationSignature = generationSignature),
        txs = txs,
        signer = signer,
        Set.empty,
        -1L
      )
      .explicitGet()

  def customBuildMicroBlockOfTxs(totalRefTo: ByteStr,
                                 prevTotal: Block,
                                 txs: Seq[Transaction],
                                 signer: KeyPair,
                                 version: Byte,
                                 ts: Long): (Block, MicroBlock) = {
    val newTotalBlock = customBuildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs, signer, version, ts)
    val nonSigned = MicroBlock
      .buildAndSign(
        generator = signer,
        transactionData = txs,
        prevResBlockSig = prevTotal.header.uniqueId,
        totalResBlockSig = newTotalBlock.header.uniqueId
      )
      .explicitGet()
    (newTotalBlock, nonSigned)
  }

  def buildMicroBlockOfTxs(totalRefTo: ByteStr, prevTotal: Block, txs: Seq[Transaction], signer: KeyPair): (Block, MicroBlock) = {
    val newTotalBlock = buildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs)
    val nonSigned = MicroBlock
      .buildAndSign(
        generator = signer,
        transactionData = txs,
        prevResBlockSig = prevTotal.header.uniqueId,
        totalResBlockSig = newTotalBlock.header.uniqueId
      )
      .explicitGet()
    (newTotalBlock, nonSigned)
  }

  def randomSig: ByteStr = TestBlock.randomOfLength(Block.BlockIdLength)

  def chainBlocks(txs: Seq[Seq[Transaction]]): Seq[Block] = {
    def chainBlocksR(refTo: ByteStr, txs: Seq[Seq[Transaction]]): Seq[Block] = txs match {
      case (x :: xs) =>
        val block = buildBlockOfTxs(refTo, x)
        block +: chainBlocksR(block.header.uniqueId, xs)
      case _ => Seq.empty
    }

    chainBlocksR(randomSig, txs)
  }

  def chainBaseAndMicro(totalRefTo: ByteStr, base: Transaction, micros: Seq[Seq[Transaction]]): (Block, Seq[MicroBlock]) =
    chainBaseAndMicro(totalRefTo, Seq(base), micros, defaultSigner, 3, base.timestamp)

  def chainBaseAndMicro(totalRefTo: ByteStr,
                        base: Seq[Transaction],
                        micros: Seq[Seq[Transaction]],
                        signer: KeyPair,
                        version: Byte,
                        timestamp: Long): (Block, Seq[MicroBlock]) = {
    val block = customBuildBlockOfTxs(totalRefTo, base, signer, version, timestamp)
    val microBlocks = micros
      .foldLeft((block, Seq.empty[MicroBlock])) {
        case ((lastTotal, allMicros), txs) =>
          val (newTotal, micro) = customBuildMicroBlockOfTxs(totalRefTo, lastTotal, txs, signer, version, timestamp)
          (newTotal, allMicros :+ micro)
      }
      ._2
    (block, microBlocks)
  }

  def spoilSignature(b: Block): Block = b.copy(header = b.header.copy(signerData = b.header.signerData.copy(signature = TestBlock.randomSignature())))
}
