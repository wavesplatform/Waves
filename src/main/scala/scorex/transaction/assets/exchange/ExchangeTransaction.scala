package scorex.transaction.assets.exchange

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.state2.ByteStr
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.{GenericError, OrderValidationError}
import scorex.transaction.{ValidationError, _}

import scala.util.{Failure, Success, Try}

case class ExchangeTransaction private(buyOrder: Order, sellOrder: Order, price: Long, amount: Long, buyMatcherFee: Long,
                                       sellMatcherFee: Long, fee: Long, timestamp: Long, signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ExchangeTransaction

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  @ApiModelProperty(hidden = true)
  override val sender: PublicKeyAccount = buyOrder.matcherPublicKey

  override val toSign: Coeval[Array[Byte]] = Coeval.evalOnce(Array(transactionType.id.toByte) ++
    Ints.toByteArray(buyOrder.bytes().length) ++ Ints.toByteArray(sellOrder.bytes().length) ++
    buyOrder.bytes() ++ sellOrder.bytes() ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
    Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
    Longs.toByteArray(timestamp))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(toSign() ++ signature.arr)

  override val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase() ++ Json.obj(
    "order1" -> buyOrder.json(),
    "order2" -> sellOrder.json(),
    "price" -> price,
    "amount" -> amount,
    "buyMatcherFee" -> buyMatcherFee,
    "sellMatcherFee" -> sellMatcherFee
  ))

  override val signedDescendants: Coeval[Seq[Order]] = Coeval.evalOnce(Seq(buyOrder, sellOrder))
}

object ExchangeTransaction {
  def create(matcher: PrivateKeyAccount, buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
             buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long): Either[ValidationError, ExchangeTransaction] = {
    create(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(EllipticCurveImpl.sign(matcher.privateKey, unverified.toSign())))
    }
  }

  def create(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
             buyMatcherFee: Long, sellMatcherFee: Long, fee: Long, timestamp: Long, signature: ByteStr): Either[ValidationError, ExchangeTransaction] = {
    lazy val priceIsValid: Boolean = price <= buyOrder.price && price >= sellOrder.price

    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (price <= 0) {
      Left(GenericError("price should be > 0"))
    } else if (price > Order.MaxAmount) {
      Left(GenericError("price too large"))
    } else if (amount > Order.MaxAmount) {
      Left(GenericError("amount too large"))
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
    } else {
      Right(ExchangeTransaction(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature))
    }
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
    val signature = ByteStr(bytes.slice(from, from + TransactionParser.SignatureLength))
    from += TransactionParser.SignatureLength

    create(o1, o2, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

}
