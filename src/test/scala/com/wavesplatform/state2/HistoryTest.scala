package com.wavesplatform.state2

import com.wavesplatform.history.HistoryWriterImpl
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.TransactionParser.SignatureLength

trait HistoryTest {
  def appendGenesisBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(EqByteArray(Array.fill(SignatureLength)(0: Byte)))).explicitGet()
  def appendTestBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(history.lastBlock.uniqueId)).explicitGet()
}
