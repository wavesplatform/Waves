package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.block._
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.crypto.EllipticCurveImpl
import scorex.settings.{Settings, ChainParameters}
import scorex.transaction.{Transaction, TransactionModule, TransactionsBlockField}
import scala.concurrent.duration._

class BlockMock(txs: Seq[Transaction], signer: PublicKeyAccount = new PublicKeyAccount(Array.fill(32)(0))) extends Block(0, 0, Array.fill(EllipticCurveImpl.SignatureLength)(0: Byte),SignerData(signer, Array.fill(EllipticCurveImpl.SignatureLength)(0))) {


  override lazy val transactions = txs
  override implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = new WavesConsensusModule(ChainParameters.Disabled, 5.seconds)

  override type ConsensusDataType = NxtLikeConsensusBlockData
  override type TransactionDataType = Seq[Transaction]

  override val transactionDataField: BlockField[TransactionDataType] = TransactionsBlockField(txs)
  override val uniqueId: BlockId = Array.fill(EllipticCurveImpl.SignatureLength)(0: Byte)

  //TODO implement mock?
  override implicit lazy val transactionModule: TransactionModule[TransactionDataType] = {
    new Error("").printStackTrace()
    throw new Error("Transaction module is not defined in mock block")
  }
  override lazy val consensusDataField: BlockField[ConsensusDataType] = NxtConsensusBlockField(
    new NxtLikeConsensusBlockData {
      val generationSignature: Array[Byte] = Array.fill(EllipticCurveImpl.SignatureLength)(0: Byte)
      val baseTarget = 1L
    })

}
