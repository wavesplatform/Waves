package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.Transaction
import scorex.transaction.TransactionParser._

import scala.util.Try

object TestBlock {
  def apply(txs: Seq[Transaction], signer: PublicKeyAccount = PublicKeyAccount(Array.fill(32)(0))): Block = apply(
    Try(txs.map(_.timestamp).max).getOrElse(0), txs, signer)

  def apply(time: Long, txs: Seq[Transaction], signer: PublicKeyAccount): Block = Block(
    time,
    0,
    Array.fill(SignatureLength)(0: Byte),
    SignerData(signer, Array.fill(EllipticCurveImpl.SignatureLength)(0)),
    NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)),
    txs)

  def create(time: Long, txs: Seq[Transaction], signer: PublicKeyAccount = PublicKeyAccount(Array.fill(32)(0))): Block = apply(time, txs, signer)

  def withReference(ref: BlockId) = Block(0, 1, ref, SignerData(PublicKeyAccount(Array.fill(32)(0)), Array.fill(SignatureLength)(0: Byte)),
    NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)), Seq.empty)

}
