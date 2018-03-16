package scorex.transaction.smart

import com.wavesplatform.lang.WavesContext
import com.wavesplatform.lang.traits.{Transaction => ContractTransaction}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{Authorized, ProvenTransaction, Transaction}

class ConsensusContext(tx: Coeval[Transaction], h: Coeval[Int], state: SnapshotStateReader) extends WavesContext {

  import ConsensusContext._

  override def height: Int = h()

  override def transaction: ContractTransaction = convert(tx())

  override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
    state
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(convert)
}

object ConsensusContext {

  def convert(tx: Transaction): ContractTransaction = new ContractTransaction {

    override def bodyBytes: Either[String, ByteVector] =  tx match {
      case pt: ProvenTransaction => Right(ByteVector(pt.bodyBytes()))
      case _                     => Left("Transaction is not Proven, doesn't contain bodyBytes")
    }

    override def transactionType: Int = tx.transactionType.id

    override def senderPk: Either[String, ByteVector] = tx match {
      case pt: Authorized => Right(ByteVector(pt.sender.publicKey))
      case _              => Left("Transaction doesn't contain sender public key")
    }

    override def assetId: Either[String, Option[ByteVector]] = tx match {
      case tt: TransferTransaction => Right(tt.assetId.map(x => ByteVector(x.arr)))
      case _                       => Left("Transaction doesn't contain asset id")
    }

    override def proofs: Either[String, IndexedSeq[ByteVector]] = tx match {
      case pt: ProvenTransaction => Right(pt.proofs.proofs.map(pf => ByteVector(pf.arr)).toIndexedSeq)
      case _                     => Left("Transaction doesn't contain proofs")
    }

    override def id: ByteVector = ByteVector(tx.id().arr)
  }
}
