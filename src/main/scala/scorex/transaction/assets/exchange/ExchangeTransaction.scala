package scorex.transaction.assets.exchange

import com.google.common.primitives.{Ints, Longs}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.BytesSerializable
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction.ValidationError.CustomValidationError
import scorex.transaction.{ValidationError, _}

import scala.util.{Failure, Success, Try}


sealed trait ExchangeTransaction extends SignedTransaction {
  def buyOrder: Order
  def sellOrder: Order
  def price: Long
  def amount: Long
  def buyMatcherFee: Long
  def sellMatcherFee: Long
  def fee: Long
}

object ExchangeTransaction {

  private case class ExchangeTransactionImpl(buyOrder: Order, sellOrder: Order, price: Long, amount: Long, buyMatcherFee: Long,
                                             sellMatcherFee: Long, fee: Long, timestamp: Long, signature: Array[Byte])
    extends ExchangeTransaction with BytesSerializable {

    override val transactionType: TransactionType.Value = TransactionType.OrderMatchTransaction

    override lazy val id: Array[Byte] = FastCryptographicHash(toSign)

    override val assetFee: (Option[AssetId], Long) = (None, fee)

    @ApiModelProperty(hidden = true)
    override val sender: PublicKeyAccount = buyOrder.matcher

    lazy val toSign: Array[Byte] = Array(transactionType.id.toByte) ++
      Ints.toByteArray(buyOrder.bytes.length) ++ Ints.toByteArray(sellOrder.bytes.length) ++
      buyOrder.bytes ++ sellOrder.bytes ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp)

    override def bytes: Array[Byte] = toSign ++ signature

    override def json: JsObject = Json.obj(
      "order1" -> buyOrder.json,
      "order2" -> sellOrder.json,
      "price" -> price,
      "amount" -> amount,
      "buyMatcherFee" -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee,
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> Base58.encode(signature)
    )

    override def balanceChanges(): Seq[BalanceChange] = {

      val matcherChange = Seq(BalanceChange(AssetAcc(buyOrder.matcher, None), buyMatcherFee + sellMatcherFee - fee))
      val buyFeeChange = Seq(BalanceChange(AssetAcc(buyOrder.sender, None), -buyMatcherFee))
      val sellFeeChange = Seq(BalanceChange(AssetAcc(sellOrder.sender, None), -sellMatcherFee))

      val exchange = Seq(
        (buyOrder.sender, (buyOrder.spendAssetId, -buyOrder.getSpendAmount(price, amount))),
        (buyOrder.sender, (buyOrder.receiveAssetId, buyOrder.getReceiveAmount(price, amount))),
        (sellOrder.sender, (sellOrder.receiveAssetId, sellOrder.getReceiveAmount(price, amount))),
        (sellOrder.sender, (sellOrder.spendAssetId, -sellOrder.getSpendAmount(price, amount)))
      )

      buyFeeChange ++ sellFeeChange ++ matcherChange ++
        exchange.map(c => BalanceChange(AssetAcc(c._1, c._2._1), c._2._2))
    }
  }

  private def createUnverified(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
      buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long, signature: Option[Array[Byte]] = None) = {
    lazy val priceIsValid: Boolean = price <= buyOrder.price && price >= sellOrder.price

    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (price <= 0) {
      Left(CustomValidationError("price should be > 0"))
    } else if (price > Order.MaxAmount) {
      Left(CustomValidationError("price too large"))
    } else if (amount > Order.MaxAmount) {
      Left(CustomValidationError("price too large"))
    } else if (sellMatcherFee > Order.MaxAmount) {
      Left(CustomValidationError("sellMatcherFee too large"))
    } else if (buyMatcherFee > Order.MaxAmount) {
      Left(CustomValidationError("buyMatcherFee too large"))
    } else if (fee > Order.MaxAmount) {
      Left(CustomValidationError("fee too large"))
    } else if (buyOrder.orderType != OrderType.BUY) {
      Left(CustomValidationError("buyOrder should has OrderType.BUY"))
    } else if (sellOrder.orderType != OrderType.SELL) {
      Left(CustomValidationError("sellOrder should has OrderType.SELL"))
    } else if (buyOrder.matcher != sellOrder.matcher) {
      Left(CustomValidationError("buyOrder.matcher should be the same as sellOrder.matcher"))
    } else if (buyOrder.assetPair != sellOrder.assetPair) {
      Left(CustomValidationError("Both orders should have same AssetPair"))
    } else if (!buyOrder.isValid(timestamp)) {
      Left(CustomValidationError("buyOrder"))
    } else if (!sellOrder.isValid(timestamp)) {
      Left(CustomValidationError("sellOrder"))
    } else if (!priceIsValid) {
      Left(CustomValidationError("priceIsValid"))
    } else {
      Right(ExchangeTransactionImpl(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature.orNull))
    }
  }

  def create(matcher: PrivateKeyAccount, buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
             buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long): Either[ValidationError, ExchangeTransaction] = {
    createUnverified(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(matcher.privateKey, unverified.toSign))
    }
  }

  def create(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
             buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long, signature: Array[Byte]): Either[ValidationError, ExchangeTransaction] = {
    createUnverified(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def parseBytes(bytes: Array[Byte]): Try[ExchangeTransaction] = Try {
    require(bytes.head == TransactionType.OrderMatchTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[ExchangeTransaction] = Try {
    import EllipticCurveImpl._
    var from = 0
    val o1Size = Ints.fromByteArray(bytes.slice(from, from + 4));
    from += 4
    val o2Size = Ints.fromByteArray(bytes.slice(from, from + 4));
    from += 4
    val o1 = Order.parseBytes(bytes.slice(from, from + o1Size)).get;
    from += o1Size
    val o2 = Order.parseBytes(bytes.slice(from, from + o2Size)).get;
    from += o2Size
    val price = Longs.fromByteArray(bytes.slice(from, from + 8));
    from += 8
    val amount = Longs.fromByteArray(bytes.slice(from, from + 8));
    from += 8
    val buyMatcherFee = Longs.fromByteArray(bytes.slice(from, from + 8));
    from += 8
    val sellMatcherFee = Longs.fromByteArray(bytes.slice(from, from + 8));
    from += 8
    val fee = Longs.fromByteArray(bytes.slice(from, from + 8));
    from += 8
    val timestamp = Longs.fromByteArray(bytes.slice(from, from + 8));
    from += 8
    val signature = bytes.slice(from, from + TypedTransaction.SignatureLength);
    from += TypedTransaction.SignatureLength

    create(o1, o2, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

}
