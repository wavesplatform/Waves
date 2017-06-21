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

  def apply(txs: Seq[Transaction]): Block = createSigned(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs)

  def createSigned(time: Long, txs: Seq[Transaction]): Block = sign(Block(
    timestamp = time,
    version = 0,
    reference = randomSignature(),
    signerData = SignerData(defaultSigner, ByteStr(Array.fill(EllipticCurveImpl.SignatureLength)(0))),
    consensusData = NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)),
    transactionData = txs))

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  def withReference(ref: ByteStr, time: Long = 0): Block = sign(Block(time, 1, ref, SignerData(defaultSigner, randomSignature()),
    NxtLikeConsensusBlockData(1L, randomSignature().arr), Seq.empty))

  def empty: Block = withReference(ByteStr(Array.fill(SignatureLength)(0: Byte)))

  private def sign(nonSignedBlock: Block): Block = {
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(defaultSigner, toSign)
    nonSignedBlock.copy(signerData = SignerData(defaultSigner, ByteStr(signature)))
  }
}
