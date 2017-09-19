package scorex.lagonaki.mocks


import com.wavesplatform.state2.ByteStr
import scorex.account.PrivateKeyAccount
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.{Try,Random}

sealed trait TestBlock {
  val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))

  val random: Random = new Random()

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  def sign(nonSignedBlock: Block): Block = {
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(defaultSigner, toSign)
    nonSignedBlock.copy(signerData = SignerData(defaultSigner, ByteStr(signature)))
  }

}

object TestBlock extends TestBlock {
  def create(txs: Seq[Transaction]): Block = create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs)

  def create(time: Long, txs: Seq[Transaction]): Block = sign(Block(
    timestamp = time,
    version = 2,
    reference = randomSignature(),
    signerData = SignerData(defaultSigner, ByteStr.empty),
    consensusData = NxtLikeConsensusBlockData(1L, ByteStr(Array.fill(SignatureLength)(0: Byte))),
    transactionData = txs,
    supportedFeaturesIds = Set.empty))


  def withReference(ref: ByteStr): Block = sign(Block(0, 1, ref, SignerData(defaultSigner, ByteStr.empty),
    NxtLikeConsensusBlockData(1L, ByteStr(randomSignature().arr)), Seq.empty, Set.empty))
}

object TestBlock3 extends TestBlock {
  def withReferenceAndFeatures(ref: ByteStr, features: Set[Short]): Block = sign(Block(0, 3, ref, SignerData(defaultSigner, ByteStr.empty),
    NxtLikeConsensusBlockData(1L, ByteStr(randomSignature().arr)), Seq.empty, features))
}
