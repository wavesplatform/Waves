package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.Transaction
import scorex.transaction.TransactionParser._

import scala.util.Try

object TestBlock {
  def apply(txs: Seq[Transaction], signer: PublicKeyAccount = PublicKeyAccount(Array.fill(32)(0))) = Block(
    Try(txs.map(_.timestamp).max).getOrElse(0), 0,
    Array.fill(SignatureLength)(0: Byte), SignerData(signer, Array.fill(EllipticCurveImpl.SignatureLength)(0)),
    NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)), txs)
}
