package scorex.transaction

import java.math.BigDecimal
import java.math.MathContext
import database.DBSet
import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.crypto.Base58
import settings.Settings
import Transaction._


abstract class Transaction(val transactionType: TransactionType.Value,
                           val fee: BigDecimal,
                           val timestamp: Long,
                           val reference: Array[Byte],
                           val signature: Array[Byte]) {

  val TypeId = transactionType.id

  lazy val deadline = timestamp + (1000 * 60 * 60 * 24) //24HOUR DEADLINE TO INCLUDE TRANSACTION IN BLOCK

  lazy val feePerByte = fee.divide(new BigDecimal(this.getDataLength()), MathContext.DECIMAL32)

  lazy val hasMinimumFee = fee.compareTo(MINIMUM_FEE) >= 0

  lazy val hasMinimumFeePerByte = {
    val minFeePerByte = BigDecimal.ONE.divide(BigDecimal.valueOf(Settings.maxBytePerFee), MathContext.DECIMAL32)
    feePerByte.compareTo(minFeePerByte) >= 0
  }

  //PARSE/CONVERT

  protected def getJsonBase() = {
    Json.obj("type" -> transactionType.id,
      "fee" -> fee.toPlainString,
      "timestamp" -> timestamp,
      "reference" -> Base58.encode(this.reference),
      "signature" -> Base58.encode(this.signature),
      "confirmations" -> getConfirmations()
    )
  }

  def toJson(): JsObject

  def toBytes(): Array[Byte]

  def getDataLength(): Int

  //VALIDATE

  def isSignatureValid(): Boolean

  def isValid(): ValidationResult.Value = isValid(DBSet.getInstance())

  def isValid(db: DBSet): ValidationResult.Value

  def process(): Unit = process(DBSet.getInstance())

  def process(db: DBSet): Unit

  def orphan(): Unit = orphan(DBSet.getInstance())

  def orphan(db: DBSet): Unit

  def getCreator(): Account

  def getInvolvedAccounts(): List[Account]

  def isInvolved(account: Account): Boolean

  def getAmount(account: Account): BigDecimal

  override def equals(other: Any) = other match {
    case tx: Transaction => signature.sameElements(tx.signature)
    case _ => false
  }

  def isConfirmed(): Boolean = isConfirmed(DBSet.getInstance())

  def isConfirmed(db: DBSet): Boolean = DBSet.getInstance().getTransactionMap.contains(this)

  def getConfirmations() = {
    //CHECK IF IN TRANSACTIONDATABASE
    if (DBSet.getInstance().getTransactionMap().contains(this)) {
      0
    } else {
      //CALCULATE CONFIRMATIONS
      val lastBlockHeight = DBSet.getInstance().getHeightMap().get(DBSet.getInstance().getBlockMap().getLastBlockSignature())
      val transactionBlockHeight = DBSet.getInstance().getHeightMap().get(DBSet.getInstance().getTransactionParentMap().getParent(this.signature))
      1 + lastBlockHeight - transactionBlockHeight
    }
  }
}

object Transaction {

  object ValidationResult extends Enumeration {
    type ValidationResult = Value

    val VALIDATE_OKE = Value(1)
    val INVALID_ADDRESS = Value(2)
    val NEGATIVE_AMOUNT = Value(3)
    val NEGATIVE_FEE = Value(4)
    val NO_BALANCE = Value(5)
    val INVALID_REFERENCE = Value(6)
  }

  //TYPES
  object TransactionType extends Enumeration {
    type Transactiontype = Value

    val GENESIS_TRANSACTION = Value(1)
    val PAYMENT_TRANSACTION = Value(2)
  }

  //MINIMUM FEE
  val MINIMUM_FEE = BigDecimal.ONE

  //PROPERTIES LENGTH
  val TYPE_LENGTH = 4
  val TIMESTAMP_LENGTH = 8
}