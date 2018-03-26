package scorex.transaction.smart

import com.wavesplatform.lang.TypeChecker.TypeCheckerContext
import com.wavesplatform.lang.WavesContext
import com.wavesplatform.lang.traits.{Transaction => ContractTransaction}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.account.AddressScheme
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class BlockchainContext(override val networkByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], state: SnapshotStateReader) extends WavesContext {

  import BlockchainContext._

  override def height: Int                      = h()
  override def transaction: ContractTransaction = convert(tx())

  override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
    state
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(convert)
}

object BlockchainContext {

  private val networkByte = AddressScheme.current.chainId

  lazy val typeCheckerContext: TypeCheckerContext =
    TypeCheckerContext.fromContext(new BlockchainContext(networkByte, Coeval(???), Coeval(???), null).build())

  def convert(tx: Transaction): ContractTransaction = new ContractTransaction {

    override def bodyBytes: Either[String, ByteVector] = tx match {
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

    override def fee: Long = tx.assetFee._2

    override def amount: Either[String, Long] = tx match {
      case g: GenesisTransaction           => Right(g.amount)
      case g: PaymentTransaction           => Right(g.amount)
      case g: IssueTransaction             => Right(g.quantity)
      case g: ReissueTransaction           => Right(g.quantity)
      case g: BurnTransaction              => Right(g.amount)
      case g: LeaseTransaction             => Right(g.amount)
      case g: TransferTransaction          => Right(g.amount)
      case g: VersionedTransferTransaction => Right(g.amount)
      case g: ExchangeTransaction          => Right(g.amount)
      case _: CreateAliasTransaction       => Left("Transaction doesn't contain amount")
      case _: SetScriptTransaction         => Left("Transaction doesn't contain amount")
      case _: MassTransferTransaction      => Left("Transaction doesn't contain amount")
      case _: LeaseCancelTransaction       => Left("Transaction doesn't contain amount")
    }

    override def feeAssetId: Option[ByteVector] =
      tx.assetFee._1.map(aid => ByteVector(aid.arr))

    override def timestamp: Long = tx.timestamp
  }
}
