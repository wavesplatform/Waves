package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.ExchangeTxSerializer
import com.wavesplatform.transaction.validation.impl.ExchangeTxValidator
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

case class ExchangeTransaction(
    version: TxVersion,
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long,
    proofs: Proofs
) extends VersionedTransaction
    with ProvenTransaction
    with TxWithFee.InWaves
    with FastHashId
    with SigProofsSwitch {

  override def builder: TransactionParser = ExchangeTransaction

  @ApiModelProperty(hidden = true)
  override val sender: PublicKey = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(ExchangeTransaction.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(ExchangeTransaction.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(ExchangeTransaction.serializer.toJson(this))

  override def checkedAssets: Seq[IssuedAsset] = {
    val pair = buyOrder.assetPair
    Seq(pair.priceAsset, pair.amountAsset) collect { case a: IssuedAsset => a }
  }
}

object ExchangeTransaction extends TransactionParser {
  implicit val validator = ExchangeTxValidator
  val serializer         = ExchangeTxSerializer

  implicit def sign(tx: ExchangeTransaction, privateKey: PrivateKey): ExchangeTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[ExchangeTransaction] =
    serializer.parseBytes(bytes)

  override type TransactionT = ExchangeTransaction

  override def classTag: ClassTag[ExchangeTransaction] = ClassTag(classOf[ExchangeTransaction])

  override def supportedVersions: Set[TxVersion] = Set(1, 2)

  val typeId: TxType = 7

  def create(
      version: TxVersion,
      buyOrder: Order,
      sellOrder: Order,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long,
      proofs: Proofs = Proofs.empty
  ): Either[ValidationError, ExchangeTransaction] =
    ExchangeTransaction(version, buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      matcher: PrivateKey,
      buyOrder: Order,
      sellOrder: Order,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, ExchangeTransaction] =
    create(version, buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs.empty).map(_.signWith(matcher))
}
