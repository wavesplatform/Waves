package com.wavesplatform.state

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.*
import com.wavesplatform.lagonaki.mocks.TestBlock

trait HistoryTest {
  val genesisBlock: Block = TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte))).block

  def getNextTestBlock(blockchain: Blockchain): Block =
    TestBlock.withReference(blockchain.lastBlockId.get).block

  def getNextTestBlockWithVotes(blockchain: Blockchain, votes: Seq[Short]): Block =
    TestBlock.withReferenceAndFeatures(blockchain.lastBlockId.get, votes).block
}
