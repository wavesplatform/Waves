package com.wavesplatform.transaction.protobuf

import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction._
import com.wavesplatform.{transaction => vt}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch

trait PBTransactionImplicits {
  class PBTransactionVanillaAdapter(tx: PBTransaction) extends VanillaTransaction with com.wavesplatform.transaction.SignedTransaction {
    def underlying: Transaction = tx

    override def timestamp: Long                   = tx.timestamp
    override val sender: PublicKeyAccount          = tx.sender
    override val proofs: Proofs                    = Proofs.empty
    override val signature: ByteStr                = proofs.toSignature
    override def builder: PBTransaction.type       = PBTransaction
    override def assetFee: (Option[ByteStr], Long) = (Some(tx.feeAssetId: ByteStr).filterNot(_.isEmpty), tx.fee)

    override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.bodyBytes()

      case _ =>
        tx.protoUnsignedBytes()
    })

    override val bytes: Coeval[Array[Byte]] = tx.protoBytes

    override val json: Coeval[JsObject] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => tx.toVanilla.json()
      case _     => ???
    })

    override val id: Coeval[ByteStr] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.id()

      case _ =>
        // PBUtils.encodeDeterministic(tx.copy(proofsArray = Nil))
        FastHashId.create(bodyBytes())
    })

    override val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
      (tx.data.isGenesis || tx.version > 1) || (tx.toVanilla match {
        case s: Signed => s.signatureValid()
        case _         => true
      })
    }

    override def equals(other: Any): Boolean = other match {
      case a: PBTransactionVanillaAdapter => tx.equals(a.underlying)
      case a: VanillaTransaction          => tx.equals(SignedTransaction.SignedVanillaTransactionImplicitConversionOps(a).toPB.transaction)
      case _                              => tx.equals(other)
    }

    // private[this] lazy val _hashCode = if (tx.version > 2) tx.hashCode() else tx.toVanilla.hashCode()
    override def hashCode(): Int = tx.hashCode() // _hashCode
  }

  implicit class VanillaOrderImplicitConversionOps(order: vt.assets.exchange.Order) {
    def toPB: ExchangeTransactionData.Order = {
      ExchangeTransactionData.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        Some(ExchangeTransactionData.Order.AssetPair(order.assetPair.amountAsset, order.assetPair.priceAsset)),
        order.orderType match {
          case vt.assets.exchange.OrderType.BUY  => ExchangeTransactionData.Order.Type.BUY
          case vt.assets.exchange.OrderType.SELL => ExchangeTransactionData.Order.Type.SELL
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.matcherFee,
        order.proofs,
        order.version
      )
    }
  }

  implicit class PBOrderImplicitConversionOps(order: ExchangeTransactionData.Order) {
    def toVanillaWithVersion(version: Int): vt.assets.exchange.Order = {
      vt.assets.exchange.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        vt.assets.exchange.AssetPair(order.getAssetPair.amountAssetId, order.getAssetPair.priceAssetId),
        order.orderType match {
          case ExchangeTransactionData.Order.Type.BUY             => vt.assets.exchange.OrderType.BUY
          case ExchangeTransactionData.Order.Type.SELL            => vt.assets.exchange.OrderType.SELL
          case ExchangeTransactionData.Order.Type.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.matcherFee,
        vt.Proofs(order.proofs),
        version
      )
    }

    def toVanilla: vt.assets.exchange.Order = toVanillaWithVersion(order.version.toInt)
  }

  implicit class PBTransactionImplicitConversionOps(tx: PBTransaction) {
    def asSigned: PBSignedTransaction = PBSignedTransaction(tx)

    def withCurrentChainId: Transaction = tx.withChainId(AddressScheme.current.chainId)

    def isLegacy: Boolean = tx.version == 1 || tx.version == 2

    def toVanillaOrAdapter: VanillaTransaction = if (this.isLegacy) toVanilla else toVanillaAdapter

    def toVanillaAdapter = new PBTransactionVanillaAdapter(tx)

    def toVanilla: VanillaTransaction = PBSignedTransactionImplicits.PBTransactionImplicitConversionOps(this.asSigned).toVanilla
  }

  private[this] implicit def implicitIntToByte(int: Int): Byte = {
    require(int >= 0 && int <= 0xFF, s"Byte overflow: $int")
    int.toByte
  }

  private[this] implicit def implicitAssetIdToOption(assetId: PBAssetId): Option[VanillaAssetId] =
    Option(assetId)
      .map(_.bytes)
      .filterNot(_.isEmpty)

  private[this] implicit def implicitAssetIdOptionToAssetId(assetId: Option[VanillaAssetId]): PBAssetId =
    assetId.fold(PBAssetId.Waves)(PBAssetId.fromBytes)
}
