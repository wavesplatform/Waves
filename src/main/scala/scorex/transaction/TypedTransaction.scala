package scorex.transaction

import scorex.serialization.Deser
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{ExchangeTransaction}

import scala.util.{Failure, Try}

trait TypedTransaction extends Transaction {

  import TypedTransaction._

  val transactionType: TransactionType.Value
}

object TypedTransaction extends Deser[TypedTransaction] {

  //TYPES
  @SerialVersionUID(-6895735531914374629L)
  object TransactionType extends Enumeration {
    val GenesisTransaction = Value(1)
    val PaymentTransaction = Value(2)
    val IssueTransaction = Value(3)
    val TransferTransaction = Value(4)
    val ReissueTransaction = Value(5)
    val BurnTransaction = Value(6)
    val OrderMatchTransaction = Value(7)
  }

  val TimestampLength = 8
  val AmountLength = 8
  val TypeLength = 1

  def parseBytes(data: Array[Byte]): Try[TypedTransaction] =
    data.head match {
      case txType: Byte if txType == TransactionType.GenesisTransaction.id =>
        GenesisTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.PaymentTransaction.id =>
        PaymentTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.IssueTransaction.id =>
        IssueTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.TransferTransaction.id =>
        TransferTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.ReissueTransaction.id =>
        ReissueTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.BurnTransaction.id =>
        BurnTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.OrderMatchTransaction.id =>
        ExchangeTransaction.parseTail(data.tail)

      case txType => Failure(new Exception(s"Invalid transaction type: $txType"))
    }
}
