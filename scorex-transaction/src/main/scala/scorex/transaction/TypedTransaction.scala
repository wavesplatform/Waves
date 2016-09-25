package scorex.transaction

import scorex.serialization.Deser
import scorex.transaction.assets.{TransferTransaction, IssueTransaction}

import scala.util.{Failure, Try}

trait TypedTransaction extends Transaction {

  import TypedTransaction._

  val transactionType: TransactionType.Value
}

object TypedTransaction extends Deser[TypedTransaction] {

  //TYPES
  object TransactionType extends Enumeration {
    val GenesisTransaction = Value(1)
    val PaymentTransaction = Value(2)
    val IssueTransaction = Value(3)
    val TransferTransaction = Value(4)
  }

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

      case txType => Failure(new Exception(s"Invalid transaction type: $txType"))
    }
}
