package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{Address, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.Order.Version
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import com.wavesplatform.transaction.serialization.impl.OrderSerializer
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject}

import scala.util.Try

sealed trait OrderAuthentication
object OrderAuthentication {
  final case class OrderProofs(key: PublicKey, proofs: Proofs) extends OrderAuthentication
  final case class Eip712Signature(signature: ByteStr)         extends OrderAuthentication

  def apply(pk: PublicKey): OrderProofs = OrderProofs(pk, Proofs.empty)
}

/** Order to matcher service for asset exchange
  */
case class Order(
    version: Version,
    orderAuthentication: OrderAuthentication,
    matcherPublicKey: PublicKey,
    assetPair: AssetPair,
    orderType: OrderType,
    amount: TxExchangeAmount,
    price: TxOrderPrice,
    timestamp: TxTimestamp,
    expiration: TxTimestamp,
    matcherFee: TxMatcherFee,
    matcherFeeAssetId: Asset = Waves,
    priceMode: OrderPriceMode = OrderPriceMode.Default,
    attachment: Option[ByteStr] = None
) extends Proven {
  import Order.*

  lazy val senderPublicKey: PublicKey = orderAuthentication match {
    case OrderAuthentication.OrderProofs(publicKey, _)  => publicKey
    case OrderAuthentication.Eip712Signature(signature) => EthOrders.recoverEthSignerKey(this, signature.arr)
  }

  val eip712Signature: Option[ByteStr] = orderAuthentication match {
    case OrderAuthentication.Eip712Signature(signature) => Some(signature)
    case OrderAuthentication.OrderProofs(_, _)          => None
  }

  val proofs: Proofs = orderAuthentication match {
    case OrderAuthentication.OrderProofs(_, proofs) => proofs
    case OrderAuthentication.Eip712Signature(_)     => Proofs.empty
  }

  lazy val sender: PublicKey = senderPublicKey
  def senderAddress: Address = sender.toAddress

  def withProofs(proofs: Proofs): Order = {
    copy(orderAuthentication = OrderAuthentication.OrderProofs(senderPublicKey, proofs))
  }

  def isValid(atTime: Long): Validation = {
    assetPair.isValid &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime" &&
    (matcherFeeAssetId == Waves || version >= Order.V3) :| "matcherFeeAssetId should be waves" &&
    (version > 0 && version < 5) :| "invalid version" &&
    (eip712Signature.isEmpty || version >= Order.V4) :| "eip712Signature available only in V4" &&
    eip712Signature.forall(es => es.size == 65 || es.size == 129) :| "eip712Signature should be of length 65 or 129" &&
    (version >= Order.V4 || priceMode == OrderPriceMode.Default) :| s"price mode should be default for V$version" &&
    (orderAuthentication match {
      case OrderAuthentication.OrderProofs(_, proofs) =>
        Proofs.validate(proofs).fold(e => Validation.failure(e.toString), _ => Validation.success)
      case _ => Validation.success
    }) &&
    (attachment.isEmpty || version >= Order.V4) :| "non-empty attachment field is allowed only for version >= V4" &&
    attachment.forall(_.size <= MaxAttachmentSize) :| s"attachment size should be <= $MaxAttachmentSize bytes" &&
    attachment.forall(!_.isEmpty) :| "attachment size should be > 0"
  }

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(OrderSerializer.bodyBytes(this))
  val id: Coeval[ByteStr]            = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  val idStr: Coeval[String]          = Coeval.evalOnce(id().toString)

  /** @note
    *   Shouldn't be used for orders >= V4
    */
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(OrderSerializer.toBytes(this))

  def getReceiveAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  def getSpendAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  val json: Coeval[JsObject] = Coeval.evalOnce(OrderSerializer.toJson(this))

  override protected def verifyFirstProof(isRideV6Activated: Boolean): Either[GenericError, Unit] =
    if (eip712Signature.isDefined) Right(()) else super.verifyFirstProof(isRideV6Activated)

  override def toString: String = {
    val matcherFeeAssetIdStr = if (version == 3) s" matcherFeeAssetId=${matcherFeeAssetId.fold("Waves")(_.toString)}," else ""
    s"OrderV$version(id=${idStr()}, sender=$senderPublicKey, matcher=$matcherPublicKey, pair=$assetPair, type=$orderType, amount=$amount, " +
      s"price=$price, priceMode=$priceMode, ts=$timestamp, exp=$expiration, fee=$matcherFee,$matcherFeeAssetIdStr, eip712Signature=$eip712Signature, proofs=$proofs)"
  }
}

object Order {
  type Id      = ByteStr
  type Version = Byte

  implicit lazy val jsonFormat: Format[Order] = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant     = 100000000L
  val MaxAmount: Long   = 100 * PriceConstant * PriceConstant
  val MaxAttachmentSize = 1024

  val V1: Version = 1.toByte
  val V2: Version = 2.toByte
  val V3: Version = 3.toByte
  val V4: Version = 4.toByte

  implicit def sign(order: Order, privateKey: PrivateKey): Order =
    order.withProofs(Proofs(crypto.sign(privateKey, order.bodyBytes())))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      assetPair: AssetPair,
      orderType: OrderType,
      amount: Long,
      price: Long,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: Long,
      matcherFeeAssetId: Asset = Asset.Waves,
      priceMode: OrderPriceMode = OrderPriceMode.Default,
      attachment: Option[ByteStr] = None
  ): Either[ValidationError, Order] =
    for {
      amount     <- TxExchangeAmount(amount)(GenericError(s"Order validation error: ${TxExchangeAmount.errMsg}"))
      price      <- TxOrderPrice(price)(GenericError(s"Order validation error: ${TxOrderPrice.errMsg}"))
      matcherFee <- TxMatcherFee(matcherFee)(GenericError(s"Order validation error: ${TxMatcherFee.errMsg}"))
    } yield {
      Order(
        version,
        OrderAuthentication(sender.publicKey),
        matcher,
        assetPair,
        orderType,
        amount,
        price,
        timestamp,
        expiration,
        matcherFee,
        matcherFeeAssetId,
        priceMode = priceMode,
        attachment = attachment
      )
        .signWith(sender.privateKey)
    }

  def buy(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: Long,
      price: Long,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: Long,
      matcherFeeAssetId: Asset = Waves,
      priceMode: OrderPriceMode = OrderPriceMode.Default,
      attachment: Option[ByteStr] = None
  ): Either[ValidationError, Order] =
    Order.selfSigned(
      version,
      sender,
      matcher,
      pair,
      OrderType.BUY,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      matcherFeeAssetId,
      priceMode,
      attachment
    )

  def sell(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: Long,
      price: Long,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: Long,
      matcherFeeAssetId: Asset = Waves,
      priceMode: OrderPriceMode = OrderPriceMode.Default,
      attachment: Option[ByteStr] = None
  ): Either[ValidationError, Order] =
    Order.selfSigned(
      version,
      sender,
      matcher,
      pair,
      OrderType.SELL,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      matcherFeeAssetId,
      priceMode,
      attachment
    )

  def parseBytes(version: Version, bytes: Array[Byte]): Try[Order] =
    OrderSerializer.parseBytes(version, bytes)
}
