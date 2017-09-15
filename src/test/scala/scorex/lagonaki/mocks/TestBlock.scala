package scorex.lagonaki.mocks

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.state2.ByteStr
import scorex.account.PrivateKeyAccount
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.Try

class TestBlock(val signer: PrivateKeyAccount) {

  private def random = ThreadLocalRandom.current()

  def create(txs: Seq[Transaction]): Block = create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs)

  def create(time: Long, txs: Seq[Transaction]): Block = sign(Block(
    timestamp = time,
    version = 2,
    reference = randomSignature(),
    signerData = SignerData(signer, ByteStr.empty),
    consensusData = NxtLikeConsensusBlockData(1L, ByteStr(Array.fill(SignatureLength)(0: Byte))),
    transactionData = txs,
    supportedFeaturesIds = Set.empty))

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  def withReference(ref: ByteStr): Block = sign(Block(0, 1, ref, SignerData(signer, ByteStr.empty),
    NxtLikeConsensusBlockData(1L, ByteStr(randomSignature().arr)), Seq.empty, Set.empty))

  private def sign(nonSignedBlock: Block): Block = {
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(signer, toSign)
    nonSignedBlock.copy(signerData = SignerData(signer, ByteStr(signature)))
  }

}

object TestBlock extends TestBlock(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)))
