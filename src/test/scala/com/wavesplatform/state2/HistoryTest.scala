package com.wavesplatform.state2

import com.wavesplatform.history.HistoryWriterImpl
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.TransactionParser.SignatureLength

trait HistoryTest {
  val genesisBlock: Block = TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte)))

  def appendBlock(history: HistoryWriterImpl, block: Block): Unit = history.appendBlock(block)(Right(BlockDiff.empty)).explicitGet()

  def appendGenesisBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(genesisBlock)(Right(BlockDiff.empty)).explicitGet()

  def appendTestBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId))(Right(BlockDiff.empty)).explicitGet()

  def appendTestBlock3(history: HistoryWriterImpl, features: Set[Short]): Unit =
    history.appendBlock(TestBlock.withReferenceAndFeatures(history.lastBlock.get.uniqueId, features))(Right(BlockDiff.empty)).explicitGet()

  def getNextTestBlock(history: HistoryWriterImpl): Block =
    TestBlock.withReference(history.lastBlock.get.uniqueId)
}
