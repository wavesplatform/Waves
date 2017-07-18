package com.wavesplatform

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{BlockchainUpdater, History, Transaction, TransactionParser}

package object history {

  case class Domain(history: History, stateReader: StateReader, blockchainUpdater: BlockchainUpdater)

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
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(applyMinerFeeWithTransactionAfter = 0))

  val ApplyMinerFeeBeforeAllTransactionsSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(applyMinerFeeWithTransactionAfter = Long.MaxValue))


  def domain(bs: BlockchainSettings = DefaultBlockchainSettings): Domain = {
    val (history, _, stateReader, blockchainUpdater) = StorageFactory(bs).get
    Domain(history, stateReader, blockchainUpdater)
  }

  val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))
  val generationSignature: Array[Byte] = Array.fill(Block.GeneratorSignatureLength)(0: Byte)

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction]): Block = {
    Block.buildAndSign(
      version = 1: Byte,
      timestamp = 0L,
      reference = refTo,
      consensusData = NxtLikeConsensusBlockData(
        baseTarget = 1L,
        generationSignature = generationSignature),
      transactionData = txs,
      signer = defaultSigner)
  }


  def buildMicroBlockOfTxs(totalRefTo: ByteStr, prevTotal: Block, txs: Seq[Transaction], signer: PrivateKeyAccount = defaultSigner): (Block, MicroBlock) = {
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

  def chainBaseAndMicro(totalRefTo: ByteStr, base: Transaction, micros: Seq[Seq[Transaction]]): (Block, Seq[MicroBlock]) = {
    val block = buildBlockOfTxs(totalRefTo, Seq(base))

    val microBlocks = micros.foldLeft((block, Seq.empty[MicroBlock])) { case ((lastTotal, allMicros), txs) =>
      val (newTotal, micro) = buildMicroBlockOfTxs(totalRefTo, lastTotal, txs)
      (newTotal, allMicros :+ micro)
    }._2
    (block, microBlocks)
  }

  def malformSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))

  trait DomainScenarioDrivenPropertyCheck extends GeneratorDrivenPropertyChecks {

    def scenario[S](gen: Gen[S], bs: BlockchainSettings = DefaultBlockchainSettings)(assertion: (Domain, S) => Assertion): Assertion = forAll(gen)(assertion(domain(bs), _))
  }

}
