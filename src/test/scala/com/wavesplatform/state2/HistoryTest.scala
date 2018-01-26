package com.wavesplatform.state2

import com.wavesplatform.TestDB
import com.wavesplatform.history.HistoryWriterImpl
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.History
import scorex.transaction.TransactionParser.SignatureLength

trait HistoryTest extends TestDB {
  val genesisBlock: Block = TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte)))

  def appendBlock(history: HistoryWriterImpl, block: Block): Unit = history.appendBlock(block, Set.empty)(Right(BlockDiff.empty)).explicitGet()

  def appendGenesisBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(genesisBlock, Set.empty)(Right(BlockDiff.empty)).explicitGet()

  def appendTestBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId), Set.empty)(Right(BlockDiff.empty)).explicitGet()

  def appendTestBlock3(history: HistoryWriterImpl, features: Set[Short]): Unit =
    history.appendBlock(TestBlock.withReferenceAndFeatures(history.lastBlock.get.uniqueId, features), Set.empty)(Right(BlockDiff.empty)).explicitGet()

  def getNextTestBlock(history: History): Block =
    TestBlock.withReference(history.lastBlock.get.uniqueId)

  def getNextTestBlockWithVotes(history: History, votes: Set[Short]): Block =
    TestBlock.withReferenceAndFeatures(history.lastBlock.get.uniqueId, votes)
}
