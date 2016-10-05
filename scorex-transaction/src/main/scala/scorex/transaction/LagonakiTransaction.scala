package scorex.transaction

import com.google.common.primitives.Ints
import play.api.libs.json.Json
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.serialization.BytesSerializable
import scorex.transaction.TypedTransaction.TransactionType

import scala.concurrent.duration._


abstract class LagonakiTransaction(val transactionType: TransactionType.Value,
                                   val recipient: Account,
                                   val amount: Long,
                                   val fee: Long,
                                   override val timestamp: Long,
                                   val signature: Array[Byte])
  extends TypedTransaction {

  import LagonakiTransaction._

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val id: Array[Byte] = signature

  lazy val deadline = timestamp + 24.hours.toMillis

  lazy val hasMinimumFee = fee >= MinimumFee

  val TypeId = transactionType.id

  //PARSE/CONVERT
  val dataLength: Int

  val creator: Option[Account]


  val signatureValid: Boolean

  //VALIDATE
  def validate: ValidationResult.Value

  def involvedAmount(account: Account): Long

  protected def jsonBase() = {
    Json.obj("type" -> transactionType.id,
      "id" -> Base58.encode(id),
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> Base58.encode(this.signature)
    )
  }
}

object LagonakiTransaction {

  val MaxBytesPerToken = 512

  //MINIMUM FEE
  val MinimumFee = 1
  val RecipientLength = Account.AddressLength
  val TypeLength = 1
  val TimestampLength = 8
  val AmountLength = 8
}
