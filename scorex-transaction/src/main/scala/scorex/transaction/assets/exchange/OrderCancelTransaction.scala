package scorex.transaction.assets.exchange

import com.google.common.primitives.Longs
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{Deser, JsonSerializable}
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction._
import scorex.transaction.assets.exchange.Validation.BooleanOperators

import scala.util.Try

/**
  * Cancel order sent to matcher
  */
case class OrderCancelTransaction(@ApiModelProperty(dataType = "java.lang.String") sender: PublicKeyAccount,
                                  @ApiModelProperty(dataType = "java.lang.String") spendAssetId: Option[AssetId],
                                  @ApiModelProperty(dataType = "java.lang.String") receiveAssetId: Option[AssetId],
                                  @ApiModelProperty(dataType = "java.lang.String") orderId: Array[Byte],
                                  @ApiModelProperty(example = "100000")fee: Long,
                                  @ApiModelProperty(example = "1481061635000") timestamp: Long,
                                  @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])
  extends SignedTransaction
  with JsonSerializable {

  @ApiModelProperty(hidden = true)
  override val transactionType: TransactionType.Value = TransactionType.OrderCancelTransaction

  @ApiModelProperty(hidden = true)
  override val typeOrdering = -10

  @ApiModelProperty(hidden = true)
  override lazy val id: Array[Byte] = FastCryptographicHash(toSign)

  @ApiModelProperty(hidden = true)
  lazy val idStr: String = Base58.encode(id)

  @ApiModelProperty(hidden = true)
  lazy val orderIdStr: String = Base58.encode(orderId)

  @ApiModelProperty(hidden = true)
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  @ApiModelProperty(hidden = true)
  lazy val assetPair: AssetPair = AssetPair(spendAssetId, receiveAssetId)

  def isValid: Validation = {
    Try{assetPair}.isSuccess :| "invalid AssetPair" &&
      signatureValid :| "signature should be valid"
  }

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = Array(transactionType.id.toByte) ++
    sender.publicKey ++
    Order.assetIdBytes(spendAssetId) ++
    Order.assetIdBytes(receiveAssetId) ++
    orderId ++
    Longs.toByteArray(fee) ++
    Longs.toByteArray(timestamp)

  @ApiModelProperty(hidden = true)
  lazy val signatureValid = EllipticCurveImpl.verify(signature, toSign, sender.publicKey)

  def bytes: Array[Byte] = toSign ++ signature

  def json: JsObject = Json.obj(
    "id" -> Base58.encode(id),
    "sender" -> Base58.encode(sender.publicKey),
    "spendAssetId" -> spendAssetId.map(Base58.encode),
    "receiveAssetId" -> receiveAssetId.map(Base58.encode),
    "orderId" -> Base58.encode(orderId),
    "fee" -> fee,
    "timestamp" -> timestamp,
    "signature" -> Base58.encode(signature)
  )

  @ApiModelProperty(hidden = true)
  override lazy val balanceChanges: Seq[BalanceChange] = Seq(BalanceChange(AssetAcc(sender, assetFee._1), -assetFee._2))

}

object OrderCancelTransaction extends Deser[OrderCancelTransaction] {

  def apply(sender: PrivateKeyAccount, spendAssetID: Option[AssetId], receiveAssetID: Option[AssetId],
            orderId: Array[Byte], fee: Long, timestamp: Long): OrderCancelTransaction = {
    val unsigned = OrderCancelTransaction(sender, spendAssetID, receiveAssetID, orderId, fee, timestamp, Array())
    sign(unsigned, sender)
  }

  def cancelOrder(sender: PrivateKeyAccount, order: Order, fee: Long, timestamp: Long): OrderCancelTransaction = {
    apply(sender, order.spendAssetId, order.receiveAssetId, order.id, fee, timestamp)
  }


  def sign(unsigned: OrderCancelTransaction, sender: PrivateKeyAccount): OrderCancelTransaction = {
    require(unsigned.sender == sender)
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  override def parseBytes(bytes: Array[Byte]): Try[OrderCancelTransaction] = Try {
    require(bytes.head == TransactionType.OrderCancelTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[OrderCancelTransaction] = Try {
    import EllipticCurveImpl._
    var from = 0
    val sender = new PublicKeyAccount(bytes.slice(from, from + KeyLength)); from += KeyLength
    val (spendAssetId, s0) = parseOption(bytes, from, AssetIdLength); from = s0
    val (receiveAssetId, s1) = parseOption(bytes, from, AssetIdLength); from = s1
    val orderId = bytes.slice(from, from + KeyLength); from += KeyLength
    val fee = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val timestamp = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val signature = bytes.slice(from, from + SignatureLength); from += SignatureLength
    OrderCancelTransaction(sender, spendAssetId, receiveAssetId, orderId, fee, timestamp, signature)
  }
}