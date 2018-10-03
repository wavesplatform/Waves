package com.wavesplatform.transaction.assets.exchange

import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.transaction.ValidationError.{GenericError, OrderValidationError}

import scala.util.Failure
import com.wavesplatform.transaction._

import scala.util.Try

trait ExchangeTransaction extends FastHashId with ProvenTransaction {
  def buyOrder: Order
  def sellOrder: Order
  def price: Long
  def amount: Long
  def buyMatcherFee: Long
  def sellMatcherFee: Long
  def fee: Long
  def timestamp: Long
  def version: Byte

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  @ApiModelProperty(hidden = true)
  override val sender: PublicKeyAccount = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]]

  override val bytes: Coeval[Array[Byte]]

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"        -> version,
      "order1"         -> buyOrder.json(),
      "order2"         -> sellOrder.json(),
      "price"          -> price,
      "amount"         -> amount,
      "buyMatcherFee"  -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee
    ))
}

object ExchangeTransaction {

  val typeId: Byte = 7

  def parse(bytes: Array[Byte]): Try[ExchangeTransaction] =
    bytes.headOption
      .fold(Failure(new Exception("Empty array")): Try[ExchangeTransaction]) { b =>
        if (b == 0) ExchangeTransactionV2.parseBytes(bytes)
        else ExchangeTransactionV1.parseBytes(bytes)
      }

  def validateExchangeParams(buyOrder: Order,
                             sellOrder: Order,
                             price: Long,
                             amount: Long,
                             buyMatcherFee: Long,
                             sellMatcherFee: Long,
                             fee: Long,
                             timestamp: Long): Either[ValidationError, Unit] = {
    lazy val priceIsValid: Boolean = price <= buyOrder.price && price >= sellOrder.price

    if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "assets"))
    } else if (amount > Order.MaxAmount) {
      Left(GenericError("amount too large"))
    } else if (price <= 0) {
      Left(GenericError("price should be > 0"))
    } else if (price > Order.MaxAmount) {
      Left(GenericError("price too large"))
    } else if (sellMatcherFee > Order.MaxAmount) {
      Left(GenericError("sellMatcherFee too large"))
    } else if (buyMatcherFee > Order.MaxAmount) {
      Left(GenericError("buyMatcherFee too large"))
    } else if (fee > Order.MaxAmount) {
      Left(GenericError("fee too large"))
    } else if (buyOrder.orderType != OrderType.BUY) {
      Left(GenericError("buyOrder should has OrderType.BUY"))
    } else if (sellOrder.orderType != OrderType.SELL) {
      Left(GenericError("sellOrder should has OrderType.SELL"))
    } else if (buyOrder.matcherPublicKey != sellOrder.matcherPublicKey) {
      Left(GenericError("buyOrder.matcher should be the same as sellOrder.matcher"))
    } else if (buyOrder.assetPair != sellOrder.assetPair) {
      Left(GenericError("Both orders should have same AssetPair"))
    } else if (!buyOrder.isValid(timestamp)) {
      Left(OrderValidationError(buyOrder, buyOrder.isValid(timestamp).messages()))
    } else if (!sellOrder.isValid(timestamp)) {
      Left(OrderValidationError(sellOrder, sellOrder.isValid(timestamp).labels.mkString("\n")))
    } else if (!priceIsValid) {
      Left(GenericError("priceIsValid"))
    } else Right(())
  }
}
