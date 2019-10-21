package com.wavesplatform.transaction.assets

import java.nio.charset.StandardCharsets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.{Asset, ProvenTransaction, VersionedTransaction}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait IssueTransaction extends ProvenTransaction with VersionedTransaction {
  def name: Array[Byte]
  def description: Array[Byte]
  def quantity: Long
  def decimals: Byte
  def reissuable: Boolean
  def fee: Long
  def script: Option[Script]

  override final val assetFee: (Asset, Long) = (Waves, fee)

  val issueJson: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"     -> version,
      "assetId"     -> id().base58,
      "name"        -> new String(name, StandardCharsets.UTF_8),
      "quantity"    -> quantity,
      "reissuable"  -> reissuable,
      "decimals"    -> decimals,
      "description" -> new String(description, StandardCharsets.UTF_8)
    )
  )

  final protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      sender,
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  )
}
object IssueTransaction {

  val typeId: Byte = 3

  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8

  def validateIssueParams(tx: IssueTransaction): Either[ValidationError, Unit] = {
    validateIssueParams(tx.name, tx.description, tx.quantity, tx.decimals, tx.reissuable, tx.fee)
  }

  def validateIssueParams(
      name: Array[Byte],
      description: Array[Byte],
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      fee: Long
  ): Either[ValidationError, Unit] = {
    //noinspection UnnecessaryPartialFunction
    (
      validateAmount(quantity, "assets"),
      validateName(name),
      validateDescription(description),
      validateDecimals(decimals),
      validateFee(fee)
    ).mapN { case _ => () }
      .leftMap(_.head)
      .toEither
  }

  implicit class IssueTransactionExt(private val tx: IssueTransaction) extends AnyVal {
    def assetId: ByteStr = tx.id()
    def isNFT: Boolean   = tx.quantity == 1 && tx.decimals == 0 && !tx.reissuable
    def isNFT(blockchain: Blockchain): Boolean = {
      import com.wavesplatform.features.BlockchainFeatures
      import com.wavesplatform.features.FeatureProvider._
      blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) && this.isNFT
    }
  }
}
