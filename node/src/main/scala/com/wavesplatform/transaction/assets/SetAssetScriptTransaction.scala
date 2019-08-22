package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class SetAssetScriptTransaction private (chainId: Byte,
                                              sender: PublicKey,
                                              asset: IssuedAsset,
                                              script: Option[Script],
                                              fee: Long,
                                              timestamp: Long,
                                              proofs: Proofs)
    extends FastHashId
    with VersionedTransaction
    with ChainSpecific {

  override val builder: TransactionParser = SetAssetScriptTransaction
  override val assetFee: (Asset, Long)    = (Waves, fee)

  override final val json: Coeval[JsObject] =
    Coeval.evalOnce(
      jsonBase() ++ Json.obj(
        "version" -> version,
        "chainId" -> chainId,
        "assetId" -> asset.id.base58,
        "script"  -> script.map(_.bytes().base64)
      )
    )

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender,
        asset.id.arr,
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp),
        Deser.serializeOptionOfArray(script)(s => s.bytes().arr)
      )
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def checkedAssets(): Seq[IssuedAsset] = Seq(asset)
  override def version: Byte               = 1
}

object SetAssetScriptTransaction extends TransactionParserFor[SetAssetScriptTransaction] with TransactionParser.MultipleVersions {

  val typeId: Byte                          = 15
  override val supportedVersions: Set[Byte] = Set(1)

  private def currentChainId: Byte = AddressScheme.current.chainId

  def create(chainId: Byte,
             sender: PublicKey,
             assetId: IssuedAsset,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {

    for {
      _ <- Either.cond(script.fold(true)(_.isInstanceOf[ExprScript]),
                       (),
                       TxValidationError.GenericError(s"Asset can only be assigned with Expression script, not Contract"))
      _ <- Either.cond(chainId == currentChainId,
                       (),
                       TxValidationError.GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
    } yield SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs)

  }

  def signed(chainId: Byte,
             sender: PublicKey,
             asset: IssuedAsset,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(chainId, sender, asset, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(chainId: Byte,
                 sender: KeyPair,
                 asset: IssuedAsset,
                 script: Option[Script],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, SetAssetScriptTransaction] = {
    signed(chainId, sender, asset, script, fee, timestamp, sender)
  }

  override def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == currentChainId,
              (),
              TxValidationError.GenericError(s"Wrong chainId actual: ${tx.chainId.toInt}, expected: $currentChainId"))
        .map(_ => tx)
        .foldToTry
    }
  }

  val byteTailDescription: ByteEntity[SetAssetScriptTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      ByteStrDefinedLength(tailIndex(3), "Asset ID", AssetIdLength),
      LongBytes(tailIndex(4), "Fee"),
      LongBytes(tailIndex(5), "Timestamp"),
      OptionBytes(index = tailIndex(6), name = "Script", nestedByteEntity = ScriptBytes(tailIndex(6), "Script")),
      ProofsBytes(tailIndex(7))
    ) mapN {
      case (chainId, sender, assetId, fee, timestamp, script, proofs) =>
        SetAssetScriptTransaction(
          chainId = chainId,
          sender = sender,
          asset = IssuedAsset(assetId),
          script = script,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
