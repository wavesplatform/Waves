package scorex.transaction

import database.PrunableBlockchainStorage
import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.crypto.Base58
import scorex.transaction.Transaction._
import settings.Settings


abstract class Transaction(val transactionType: TransactionType.Value,
                           val fee: BigDecimal,
                           val timestamp: Long,
                           val signature: Array[Byte]) {

  lazy val deadline = timestamp + (1000 * 60 * 60 * 24)
  //24HOUR DEADLINE TO INCLUDE TRANSACTION IN BLOCK
  lazy val feePerByte = fee / BigDecimal(dataLength)
  lazy val hasMinimumFee = fee >= MINIMUM_FEE
  lazy val hasMinimumFeePerByte = {
    val minFeePerByte = BigDecimal(1) / BigDecimal.valueOf(Settings.maxBytePerFee)
    feePerByte >= minFeePerByte
  }
  val TypeId = transactionType.id

  //PARSE/CONVERT
  val dataLength: Int

  def toJson(): JsObject

  def toBytes(): Array[Byte]

  def isSignatureValid(): Boolean

  //VALIDATE

  def isValid(): ValidationResult.Value

  def process(): Unit //todo: remove

  def orphan(): Unit //todo: remove

  def getCreator(): Option[Account]

  def getInvolvedAccounts(): List[Account]

  def isInvolved(account: Account): Boolean

  def getAmount(account: Account): BigDecimal

  override def equals(other: Any) = other match {
    case tx: Transaction => signature.sameElements(tx.signature)
    case _ => false
  }

  def isConfirmed(): Boolean = PrunableBlockchainStorage.confirmations(this).isDefined

  protected def getJsonBase() = {
    Json.obj("type" -> transactionType.id,
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> Base58.encode(this.signature),
      "confirmations" -> getConfirmations()
    )
  }

  def getConfirmations() = PrunableBlockchainStorage.confirmations(this)
}

object Transaction {

  //MINIMUM FEE
  val MINIMUM_FEE = BigDecimal(1)
  //PROPERTIES LENGTH
  val TYPE_LENGTH = 1
  val TIMESTAMP_LENGTH = 8
  val AMOUNT_LENGTH = 8

  def fromBytes(data: Array[Byte]): Transaction = {
    val txType = data.head

    txType match {
      case i: Byte if i == TransactionType.GENESIS_TRANSACTION.id =>
        GenesisTransaction.Parse(data.tail)

      case i: Byte if i == TransactionType.PAYMENT_TRANSACTION.id =>
        PaymentTransaction.Parse(data.tail)

      case _ => throw new Exception(s"Invalid transaction type: $txType")
    }
  }

  object ValidationResult extends Enumeration {
    type ValidationResult = Value

    val VALIDATE_OKE = Value(1)
    val INVALID_ADDRESS = Value(2)
    val NEGATIVE_AMOUNT = Value(3)
    val NEGATIVE_FEE = Value(4)
    val NO_BALANCE = Value(5)
  }

  //TYPES
  object TransactionType extends Enumeration {
    type Transactiontype = Value

    val GENESIS_TRANSACTION = Value(1)
    val PAYMENT_TRANSACTION = Value(2)
  }

}