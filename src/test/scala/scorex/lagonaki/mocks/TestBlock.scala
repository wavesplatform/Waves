package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.Transaction
import scorex.transaction.TypedTransaction._

object TestBlock {
  def apply(txs: Seq[Transaction], signer: PublicKeyAccount = new PublicKeyAccount(Array.fill(32)(0))) = Block(0, 0,
    Array.fill(SignatureLength)(0: Byte), SignerData(signer, Array.fill(EllipticCurveImpl.SignatureLength)(0)),
    NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)), txs)
}
