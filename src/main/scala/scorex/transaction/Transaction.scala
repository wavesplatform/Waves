package scorex.transaction

import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.crypto.Base58
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.transaction.Transaction._
import settings.Settings


abstract class Transaction(val transactionType: TransactionType.Value,
                           override val recipient: Account,
                           override val amount: BigDecimal,
                           val fee: BigDecimal,
                           val timestamp: Long,
                           val signature: Array[Byte]) extends PreTransaction(recipient, amount) {

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

  def getCreator(): Option[Account]

  def getInvolvedAccounts(): List[Account]

  def isInvolved(account: Account): Boolean

  override def equals(other: Any) = other match {
    case tx: Transaction => signature.sameElements(tx.signature)
    case _ => false
  }

  protected def getJsonBase() = {
    Json.obj("type" -> transactionType.id,
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> Base58.encode(this.signature)
    )
  }
}

object Transaction {

  //MINIMUM FEE
  val MINIMUM_FEE = BigDecimal(1)
  //PROPERTIES LENGTH
  val RECIPIENT_LENGTH = Account.ADDRESS_LENGTH
  val TYPE_LENGTH = 1
  val TIMESTAMP_LENGTH = 8
  val AMOUNT_LENGTH = 8

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

  def fromBytes(data: Array[Byte]): Transaction = data.head match {
    case txType: Byte if txType == TransactionType.GENESIS_TRANSACTION.id =>
      GenesisTransaction.Parse(data.tail)

    case txType: Byte if txType == TransactionType.PAYMENT_TRANSACTION.id =>
      PaymentTransaction.Parse(data.tail)

    case txType => throw new Exception(s"Invalid transaction type: $txType")
  }
}