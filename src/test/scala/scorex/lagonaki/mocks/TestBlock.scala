package scorex.lagonaki.mocks

import com.wavesplatform.state2.ByteStr
import scorex.account.PrivateKeyAccount
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.{Random, Try}

object TestBlock {

  private val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))

  private val random: Random = new Random(10)

  def create(txs: Seq[Transaction]): Block = create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs)

  def create(time: Long, txs: Seq[Transaction]): Block = sign(Block(
    timestamp = time,
    version = 0,
    reference = randomSignature(),
    signerData = SignerData(defaultSigner, ByteStr.empty),
    consensusData = NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)),
    transactionData = txs))

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  def withReference(ref: ByteStr): Block = sign(Block(0, 1, ref, SignerData(defaultSigner, ByteStr.empty),
    NxtLikeConsensusBlockData(1L, randomSignature().arr), Seq.empty))

  private def sign(nonSignedBlock: Block): Block = {
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(defaultSigner, toSign)
    nonSignedBlock.copy(signerData = SignerData(defaultSigner, ByteStr(signature)))
  }
}
