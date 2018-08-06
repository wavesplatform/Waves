package com.wavesplatform.transaction.assets.exchange

import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import com.wavesplatform.account.PublicKeyAccount

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
}
