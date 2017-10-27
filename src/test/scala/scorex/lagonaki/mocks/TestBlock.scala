package scorex.lagonaki.mocks


import com.wavesplatform.state2._
import scorex.account.PrivateKeyAccount
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.TransactionParser._
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.{Random, Try}

object TestBlock {
  val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))

  val random: Random = new Random()

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  private def sign(signer: PrivateKeyAccount, b: Block): Block = {
    Block.buildAndSign(version = b.version, timestamp = b.timestamp, reference = b.reference,
      consensusData = b.consensusData, transactionData = b.transactionData,
      signer = signer, featureVotes = b.featureVotes).explicitGet()
  }

  def create(txs: Seq[Transaction]): Block = create(defaultSigner, txs)

  def create(signer: PrivateKeyAccount, txs: Seq[Transaction]): Block = create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs, signer = signer)

  def create(time: Long, txs: Seq[Transaction]): Block = create(time, randomSignature(), txs, defaultSigner)

  def create(time: Long, txs: Seq[Transaction], signer: PrivateKeyAccount): Block = create(time, randomSignature(), txs, signer)

  def create(time: Long, ref: ByteStr, txs: Seq[Transaction], signer: PrivateKeyAccount = defaultSigner): Block = sign(signer, Block(
    timestamp = time,
    version = 2,
    reference = ref,
    signerData = SignerData(signer, ByteStr.empty),
    consensusData = NxtLikeConsensusBlockData(1L, ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))),
    transactionData = txs,
    featureVotes = Set.empty))

  def withReference(ref: ByteStr): Block = sign(defaultSigner, Block(0, 1, ref, SignerData(defaultSigner, ByteStr.empty),
    NxtLikeConsensusBlockData(1L, randomOfLength(Block.GeneratorSignatureLength)), Seq.empty, Set.empty))

  def withReferenceAndFeatures(ref: ByteStr, features: Set[Short]): Block = sign(defaultSigner, Block(0, 3, ref, SignerData(defaultSigner, ByteStr.empty),
    NxtLikeConsensusBlockData(1L, randomOfLength(Block.GeneratorSignatureLength)), Seq.empty, features))
}
