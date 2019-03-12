package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class SetAssetScriptTransaction private (chainId: Byte,
                                              sender: PublicKeyAccount,
                                              assetId: ByteStr,
                                              script: Option[Script],
                                              fee: Long,
                                              timestamp: Long,
                                              proofs: Proofs)
    extends FastHashId
    with VersionedTransaction
    with ChainSpecific {

  override val builder: TransactionParser        = SetAssetScriptTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] =
    Coeval.evalOnce(
      jsonBase() ++ Json.obj(
        "version" -> version,
        "chainId" -> chainId,
        "assetId" -> assetId.base58,
        "script"  -> script.map(_.bytes().base64)
      )
    )

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender.publicKey,
        assetId.arr,
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp),
        Deser.serializeOption(script)(s => s.bytes().arr)
      )
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def checkedAssets(): Seq[AssetId] = Seq(assetId)
  override def version: Byte                 = 1
}

object SetAssetScriptTransaction extends TransactionParserFor[SetAssetScriptTransaction] with TransactionParser.MultipleVersions {

  val typeId: Byte                          = 15
  override val supportedVersions: Set[Byte] = Set(1)

  private def currentChainId: Byte = AddressScheme.current.chainId

  def create(chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {

    for {
      _ <- Either.cond(script.fold(true)(_.isInstanceOf[ExprScript]),
                       (),
                       ValidationError.GenericError(s"Asset can oly be assigned with Expression script, not Contract"))
      _ <- Either.cond(chainId == currentChainId,
                       (),
                       ValidationError.GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
    } yield SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs)

  }

  def signed(chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(chainId, sender, assetId, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }
  override def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == currentChainId, (), ValidationError.GenericError(s"Wrong chainId actual: ${tx.chainId.toInt}, expected: $currentChainId"))
        .map(_ => tx)
        .foldToTry
    }
  }

  val byteTailDescription: ByteEntity[SetAssetScriptTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyAccountBytes(tailIndex(2), "Sender's public key"),
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
          assetId = assetId,
          script = script,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
