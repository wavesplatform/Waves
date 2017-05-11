package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.Transaction
import scorex.transaction.TransactionParser._

import scala.util.{Random, Try}

object TestBlock {
  def apply(txs: Seq[Transaction], signer: PublicKeyAccount = PublicKeyAccount(Array.fill(32)(0))): Block = apply(
    Try(txs.map(_.timestamp).max).getOrElse(0), txs, signer)

  def apply(time: Long, txs: Seq[Transaction], signer: PublicKeyAccount): Block = Block(
    time,
    0,
    randomSignature,
    SignerData(signer, Array.fill(EllipticCurveImpl.SignatureLength)(0)),
    NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)),
    txs)

  def create(time: Long, txs: Seq[Transaction], signer: PublicKeyAccount = PublicKeyAccount(Array.fill(32)(0))): Block = apply(time, txs, signer)


  def randomSignature = Array.fill(SignatureLength)(random.nextInt().toByte)
  private val random = new Random(10)
  def withReference(ref: BlockId, time: Long = 0) = Block(time, 1, ref, SignerData(PublicKeyAccount(Array.fill(32)(0)), randomSignature),
    NxtLikeConsensusBlockData(1L, randomSignature), Seq.empty)

  def empty = withReference(Array.fill(SignatureLength)(0: Byte))
}
