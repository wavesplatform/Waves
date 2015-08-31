package scorex.app

import org.joda.time.DateTime
import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.block._
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.consensus.qora.{QoraConsensusBlockField, QoraLikeConsensusBlockData, QoraLikeConsensusModule}
import scorex.crypto.SigningFunctionsImpl
import scorex.settings.Settings
import scorex.transaction._


trait LagonakiBlock extends Block {

  implicit val settings: TransactionSettings

  val signature: Block.BlockId

  override val versionField = ByteBlockField("version", 1: Byte)

  override implicit val transactionModule = new SimpleTransactionModule()(settings, consensusModule)

  override val uniqueId = signature

  // todo: make changeable
  override type TT = Seq[Transaction]
}

object NxtLagonakiBlockBuilder extends BlockBuilder[NxtLikeConsensusBlockData, Seq[Transaction]] {
  override val version = 1: Byte

  override def build(timestamp: Long,
                     reference: BlockId,
                     consensusData: NxtLikeConsensusBlockData,
                     transactionData: Seq[Transaction],
                     generator: PublicKeyAccount,
                     signature: Array[Byte])
                    (implicit consensusModule: ConsensusModule[NxtLikeConsensusBlockData],
                     transactionModule: TransactionModule[Seq[Transaction]]):
  Block {type TT = Seq[Transaction]; type CT = NxtLikeConsensusBlockData} = {
    implicit val settings = transactionModule.asInstanceOf[SimpleTransactionModule].settings //todo: asInstanceOf
    new NxtLagonakiBlock(timestamp, reference, consensusData, transactionData, generator, signature)
  }
}

class NxtLagonakiBlock(val timestamp: Long,
                       val reference: Block.BlockId,
                       val nxtConsensusData: NxtLikeConsensusBlockData,
                       val blockTransactions: Seq[Transaction],
                       val generator: PublicKeyAccount,
                       val signature: Array[Byte])
                      (implicit override val settings: TransactionSettings)
  extends LagonakiBlock {

  override type CT = NxtLikeConsensusBlockData
  override implicit val consensusModule = new NxtLikeConsensusModule

  override val timestampField = new LongBlockField("timestamp", timestamp)
  override val referenceField = new BlockIdField("reference", reference)

  override val consensusDataField = NxtConsensusBlockField(nxtConsensusData)
  override val transactionDataField = TransactionsBlockField(blockTransactions)
  override val signerDataField = new SignerDataBlockField("signature", SignerData(generator, signature))
}

class QoraLagonakiBlock(val timestamp: Long,
                        val reference: Block.BlockId,
                        val qoraConsensusData: QoraLikeConsensusBlockData,
                        val blockTransactions: Seq[Transaction],
                        val generator: PublicKeyAccount,
                        val signature: Array[Byte])
                       (implicit override val settings: Settings with TransactionSettings)
  extends LagonakiBlock {

  override type CT = QoraLikeConsensusBlockData
  override implicit val consensusModule = new QoraLikeConsensusModule

  override val timestampField = new LongBlockField("timestamp", timestamp)
  override val referenceField = new BlockIdField("reference", reference)

  override val consensusDataField = QoraConsensusBlockField(qoraConsensusData)
  override val transactionDataField = TransactionsBlockField(blockTransactions)
  override val signerDataField = new SignerDataBlockField("signature", SignerData(generator, signature))
}


class LagonakiGenesisNxt()(override implicit val consensusModule: NxtLikeConsensusModule,
                           override implicit val transactionModule: SimpleTransactionModule,
                           implicit override val settings: Settings with TransactionSettings)
  extends NxtLagonakiBlock(
    timestamp = new DateTime(System.currentTimeMillis()).toDateMidnight.getMillis,
    reference = Array.fill(SigningFunctionsImpl.SignatureLength)(0),
    nxtConsensusData = consensusModule.genesisData.value,
    blockTransactions = transactionModule.genesisData.value,
    generator = new PublicKeyAccount(Array.fill(32)(0)),
    signature = Array.fill(SigningFunctionsImpl.SignatureLength)(0)
  )

class LagonakiGenesisQora(override implicit val consensusModule: QoraLikeConsensusModule,
                          override val transactionModule: SimpleTransactionModule,
                          implicit override val settings: Settings with TransactionSettings)
  extends QoraLagonakiBlock(
    timestamp = new DateTime(System.currentTimeMillis()).toDateMidnight.getMillis,
    reference = Array(),
    qoraConsensusData = consensusModule.genesisData.value,
    blockTransactions = transactionModule.genesisData.value,
    generator = new PublicKeyAccount(Array.fill(32)(0)),
    signature = Array()
  )