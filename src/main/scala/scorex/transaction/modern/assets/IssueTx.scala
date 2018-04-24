package scorex.transaction.modern.assets

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.serialization.Deser
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.smart.script.{Script, ScriptReader}
import scorex.transaction.validation._
import scorex.transaction.{AssetId, Proofs, TransactionParser}

import scala.util.Try

final case class IssuePayload(chainId: Byte,
                              name: Array[Byte],
                              description: Array[Byte],
                              quantity: Long,
                              decimals: Byte,
                              reissuable: Boolean,
                              script: Option[Script])
    extends TxData {

  override def bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(chainId),
      Deser.serializeArray(name),
      Deser.serializeArray(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Deser.serializeOption(script)(s => s.bytes().arr)
    )
  }

  override def json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "name"        -> new String(name, StandardCharsets.UTF_8),
      "description" -> new String(description, StandardCharsets.UTF_8),
      "quantity"    -> quantity,
      "reissuable"  -> reissuable,
      "decimals"    -> decimals,
      "script"      -> script.map(_.text)
    )
  }
}

final case class IssueTx(header: TxHeader, payload: IssuePayload, proofs: Proofs) extends ModernTransaction(IssueTx) {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)

  val assetId: Coeval[AssetId] = id
}

object IssueTx extends TransactionParser.Modern[IssueTx, IssuePayload] {
  override val typeId: Byte = 3

  override val supportedVersions: Set[Byte] = Set(3)

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(IssuePayload, Int)] = Try {
    val chainId                       = bytes(0)
    val (assetName, descriptionStart) = Deser.parseArraySize(bytes, 1)
    val (description, quantityStart)  = Deser.parseArraySize(bytes, descriptionStart)
    val quantity                      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val decimals                      = bytes.slice(quantityStart + 8, quantityStart + 9).head
    val reissuable                    = bytes.slice(quantityStart + 9, quantityStart + 10).head == (1: Byte)
    val (scriptOptEi: Option[Either[ValidationError, Script]], scriptEnd) =
      Deser.parseOption(bytes, quantityStart + 10)(ScriptReader.fromBytes)
    val scriptEiOpt: Either[ValidationError, Option[Script]] = scriptOptEi match {
      case None            => Right(None)
      case Some(Right(sc)) => Right(Some(sc))
      case Some(Left(err)) => Left(err)
    }

    ValidateModern
      .issuePL(chainId, assetName, description, quantity, decimals, reissuable, scriptEiOpt.right.get)
      .fold(
        errs => throw new Exception(errs.toString),
        succ => (succ, scriptEnd)
      )
  }

  override def create(header: TxHeader, data: IssuePayload, proofs: Proofs): Try[IssueTx] = {
    Try(IssueTx(header, data, proofs))
  }
}
