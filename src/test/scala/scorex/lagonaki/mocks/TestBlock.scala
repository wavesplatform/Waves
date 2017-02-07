package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.block._
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.crypto.EllipticCurveImpl
import scorex.settings.ChainParameters
import scorex.transaction.TypedTransaction._
import scorex.transaction.{Transaction, TransactionModule, TransactionsBlockField}

import scala.concurrent.duration._

object TestBlock {
  def apply(txs: Seq[Transaction], signer: PublicKeyAccount = new PublicKeyAccount(Array.fill(32)(0))) = Block(0, 0,
    Array.fill(SignatureLength)(0: Byte), SignerData(signer, Array.fill(EllipticCurveImpl.SignatureLength)(0)),
    NxtConsensusBlockField(NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte))), TransactionsBlockField(txs))
}
