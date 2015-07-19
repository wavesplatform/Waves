package scorex.transaction

import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.crypto.Base58
import scorex.transaction.Transaction.{ValidationResult, _}
import scorex.settings.Settings


abstract class Transaction(val transactionType: TransactionType.Value,
                           val recipient: Account,
                           val amount: Long,
                           val fee: Long,
                           val timestamp: Long,
                           val signature: Array[Byte]) {

  //24HOUR DEADLINE TO INCLUDE TRANSACTION IN BLOCK
  lazy val deadline = timestamp + (1000 * 60 * 60 * 24)

  lazy val feePerByte = fee / BigDecimal(dataLength)
  lazy val hasMinimumFee = fee >= MinimumFee
  lazy val hasMinimumFeePerByte = {
    val minFeePerByte = BigDecimal(1) / BigDecimal.valueOf(Settings.maxBytePerFee)
    feePerByte >= minFeePerByte
  }

  val TypeId = transactionType.id

  //PARSE/CONVERT
  val dataLength: Int

  def json(): JsObject

  def bytes(): Array[Byte]

  def isSignatureValid(): Boolean

  //VALIDATE

  def validate(): ValidationResult.Value

  def getCreator(): Option[Account]

  def involvedAmount(account: Account): BigDecimal

  //if key is None then balance change is going to block forger
  def balanceChanges(): Map[Option[Account], BigDecimal]

  override def equals(other: Any) = other match {
    case tx: Transaction => signature.sameElements(tx.signature)
    case _ => false
  }

  protected def jsonBase() = {
    Json.obj("type" -> transactionType.id,
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> Base58.encode(this.signature)
    )
  }
}

object Transaction {

  //MINIMUM FEE
  val MinimumFee = 1
  val RecipientLength = Account.AddressLength
  val TypeLength = 1
  val TimestampLength = 8
  val AmountLength = 8

  object ValidationResult extends Enumeration {
    type ValidationResult = Value

    val ValidateOke = Value(1)
    val InvalidAddress = Value(2)
    val NegativeAmount = Value(3)
    val NegativeFee = Value(4)
    val NoBalance = Value(5)
  }

  //TYPES
  object TransactionType extends Enumeration {
    val GenesisTransaction = Value(1)
    val PaymentTransaction = Value(2)
  }

  def parse(data: Array[Byte]): Transaction = data.head match {
    case txType: Byte if txType == TransactionType.GenesisTransaction.id =>
      GenesisTransaction.parse(data.tail)

    case txType: Byte if txType == TransactionType.PaymentTransaction.id =>
      PaymentTransaction.parse(data.tail)

    case txType => throw new Exception(s"Invalid transaction type: $txType")
  }
}