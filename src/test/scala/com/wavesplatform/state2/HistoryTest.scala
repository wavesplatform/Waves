package com.wavesplatform.state2

import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.History
import scorex.transaction.TransactionParser.SignatureLength

trait HistoryTest {
  val genesisBlock: Block = TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte)))

  def getNextTestBlock(history: History): Block =
    TestBlock.withReference(history.lastBlock.get.uniqueId)

  def getNextTestBlockWithVotes(history: History, votes: Set[Short]): Block =
    TestBlock.withReferenceAndFeatures(history.lastBlock.get.uniqueId, votes)
}
