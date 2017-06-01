package scorex.transaction.assets.exchange

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.state2.{ByteArray, EqByteArray}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.serialization.BytesSerializable
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.TransactionParameterValidationError
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
                                             sellMatcherFee: Long, fee: Long, timestamp: Long, signature: ByteArray)
    extends ExchangeTransaction with BytesSerializable {

    override val transactionType: TransactionType.Value = TransactionType.ExchangeTransaction

    override val assetFee: (Option[AssetId], Long) = (None, fee)

    @ApiModelProperty(hidden = true)
    override val sender: PublicKeyAccount = buyOrder.matcherPublicKey

    lazy val toSign: Array[Byte] = Array(transactionType.id.toByte) ++
      Ints.toByteArray(buyOrder.bytes.length) ++ Ints.toByteArray(sellOrder.bytes.length) ++
      buyOrder.bytes ++ sellOrder.bytes ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp)

    override def bytes: Array[Byte] = toSign ++ signature.arr

    override def json: JsObject = jsonBase() ++ Json.obj(
      "order1" -> buyOrder.json,
      "order2" -> sellOrder.json,
      "price" -> price,
      "amount" -> amount,
      "buyMatcherFee" -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee
    )
  }

  private def createUnverified(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
                               buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long, signature: Option[EqByteArray] = None) = {
    lazy val priceIsValid: Boolean = price <= buyOrder.price && price >= sellOrder.price

    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (price <= 0) {
      Left(TransactionParameterValidationError("price should be > 0"))
    } else if (price > Order.MaxAmount) {
      Left(TransactionParameterValidationError("price too large"))
    } else if (amount > Order.MaxAmount) {
      Left(TransactionParameterValidationError("amount too large"))
    } else if (sellMatcherFee > Order.MaxAmount) {
      Left(TransactionParameterValidationError("sellMatcherFee too large"))
    } else if (buyMatcherFee > Order.MaxAmount) {
      Left(TransactionParameterValidationError("buyMatcherFee too large"))
    } else if (fee > Order.MaxAmount) {
      Left(TransactionParameterValidationError("fee too large"))
    } else if (buyOrder.orderType != OrderType.BUY) {
      Left(TransactionParameterValidationError("buyOrder should has OrderType.BUY"))
    } else if (sellOrder.orderType != OrderType.SELL) {
      Left(TransactionParameterValidationError("sellOrder should has OrderType.SELL"))
    } else if (buyOrder.matcherPublicKey != sellOrder.matcherPublicKey) {
      Left(TransactionParameterValidationError("buyOrder.matcher should be the same as sellOrder.matcher"))
    } else if (buyOrder.assetPair != sellOrder.assetPair) {
      Left(TransactionParameterValidationError("Both orders should have same AssetPair"))
    } else if (!buyOrder.isValid(timestamp)) {
      Left(TransactionParameterValidationError(buyOrder.isValid(timestamp).labels.mkString("\n")))
    } else if (!sellOrder.isValid(timestamp)) {
      Left(TransactionParameterValidationError(sellOrder.isValid(timestamp).labels.mkString("\n")))
    } else if (!priceIsValid) {
      Left(TransactionParameterValidationError("priceIsValid"))
    } else {
      Right(ExchangeTransactionImpl(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature.orNull))
    }
  }

  def create(matcher: PrivateKeyAccount, buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
             buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long): Either[ValidationError, ExchangeTransaction] = {
    createUnverified(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp).right.map { unverified =>
      unverified.copy(signature = EqByteArray(EllipticCurveImpl.sign(matcher.privateKey, unverified.toSign)))
    }
  }

  def create(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
             buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long, signature: ByteArray): Either[ValidationError, ExchangeTransaction] = {
    createUnverified(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def parseBytes(bytes: Array[Byte]): Try[ExchangeTransaction] = Try {
    require(bytes.head == TransactionType.ExchangeTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[ExchangeTransaction] = Try {
    var from = 0
    val o1Size = Ints.fromByteArray(bytes.slice(from, from + 4))
    from += 4
    val o2Size = Ints.fromByteArray(bytes.slice(from, from + 4))
    from += 4
    val o1 = Order.parseBytes(bytes.slice(from, from + o1Size)).get
    from += o1Size
    val o2 = Order.parseBytes(bytes.slice(from, from + o2Size)).get
    from += o2Size
    val price = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val amount = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val buyMatcherFee = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val sellMatcherFee = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val fee = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val timestamp = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val signature = EqByteArray(bytes.slice(from, from + TransactionParser.SignatureLength))
    from += TransactionParser.SignatureLength

    create(o1, o2, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

}
