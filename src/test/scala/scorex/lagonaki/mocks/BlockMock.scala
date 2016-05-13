package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.block._
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.{Transaction, TransactionModule, TransactionsBlockField}

class BlockMock(txs: Seq[Transaction]) extends Block {


  override lazy val transactions = txs
  override implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = new NxtLikeConsensusModule
  override val signerDataField: SignerDataBlockField = new SignerDataBlockField("signature",
    SignerData(new PublicKeyAccount(Array.fill(32)(0)), Array.fill(EllipticCurveImpl.SignatureLength)(0)))

  override type ConsensusDataType = NxtLikeConsensusBlockData
  override type TransactionDataType = Seq[Transaction]
  override val versionField: ByteBlockField = ByteBlockField("version", 0: Byte)


  override val transactionDataField: BlockField[TransactionDataType] = TransactionsBlockField(txs)
  override val referenceField: BlockIdField =
    BlockIdField("reference", Array.fill(EllipticCurveImpl.SignatureLength)(0: Byte))
  override val uniqueId: BlockId = Array.fill(EllipticCurveImpl.SignatureLength)(0: Byte)
  override val timestampField: LongBlockField = LongBlockField("timestamp", 0L)

  //TODO implement mock?
  override implicit lazy val transactionModule: TransactionModule[TransactionDataType] = {
    new Error("").printStackTrace()
    throw new Error("Transaction module is not defined in mock block")
  }
  override lazy val consensusDataField: BlockField[ConsensusDataType] = ???

}
