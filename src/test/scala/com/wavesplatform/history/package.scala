package com.wavesplatform

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.block.{Block}
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

  def domain(): Domain = {
    val (history, _, stateReader, blockchainUpdater) = StorageFactory(DefaultBlockchainSettings).get
    Domain(history, stateReader, blockchainUpdater)
  }

  private val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))
  private val generationSignature = Array.fill(Block.GeneratorSignatureLength)(0: Byte)

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

  def malformSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))

  trait DomainScenarioDrivenPropertyCheck extends GeneratorDrivenPropertyChecks {

    def scenario[S](gen: Gen[S])(assertion: (Domain, S) => Assertion): Assertion = forAll(gen)(assertion(domain(), _))
  }
}
