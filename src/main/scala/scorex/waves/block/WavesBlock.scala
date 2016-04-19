package scorex.waves.block

import scorex.account.PublicKeyAccount
import scorex.block._
import scorex.consensus.ConsensusModule
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionModule
import scorex.utils.ScorexLogging

object WavesBlock extends ScorexLogging {
  type BlockId = Array[Byte]

  val BlockIdLength = EllipticCurveImpl.SignatureLength

  /**
    * We override Block.genesis to provide correct genesis' timestamp
    */
  def genesis[CDT, TDT]()(implicit consModule: ConsensusModule[CDT],
                          transModule: TransactionModule[TDT]): Block = new Block {
    override type ConsensusDataType = CDT
    override type TransactionDataType = TDT

    override implicit val transactionModule: TransactionModule[TDT] = transModule
    override implicit val consensusModule: ConsensusModule[CDT] = consModule

    override val versionField: ByteBlockField = ByteBlockField("version", 1)
    override val transactionDataField: BlockField[TDT] = transactionModule.genesisData
    override val referenceField: BlockIdField = BlockIdField("reference", Array.fill(BlockIdLength)(0: Byte))
    override val consensusDataField: BlockField[CDT] = consensusModule.genesisData
    override val uniqueId: BlockId = Array.fill(BlockIdLength)(0: Byte)

    //todo: inject timestamp from settings
    override val timestampField: LongBlockField = LongBlockField("timestamp", 1460678400000L)

    override val signerDataField: SignerDataBlockField = new SignerDataBlockField("signature",
      SignerData(new PublicKeyAccount(Array.fill(32)(0)), Array.fill(EllipticCurveImpl.SignatureLength)(0)))
  }
}