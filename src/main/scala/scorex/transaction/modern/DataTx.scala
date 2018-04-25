package scorex.transaction.modern

import com.google.common.primitives.{Bytes, Shorts}
import com.wavesplatform.state.DataEntry
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction.base.DataTxBase
import scorex.transaction.validation.ValidateModern
import scorex.transaction.{AssetId, Proofs, TransactionParser}

import scala.util.{Failure, Success, Try}

final case class DataPayload(entries: List[DataEntry[_]]) extends TxData {
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Shorts.toByteArray(entries.length.toShort),
      entries.flatMap(_.toBytes).toArray
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj("data" -> Json.toJson(entries))
  }
}

final case class DataTx(header: TxHeader, payload: DataPayload, proofs: Proofs)
  extends ModernTransaction(DataTx)
    with DataTxBase {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)
  override val data: List[DataEntry[_]] = payload.entries
}

object DataTx extends TransactionParser.Modern[DataTx, DataPayload] {
  override val typeId: Byte = 12

  override val supportedVersions: Set[Byte] = Set(2)

  override def create(header: TxHeader, data: DataPayload, proofs: Proofs): Try[DataTx] =
    Try(DataTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(DataPayload, Int)] = {
    for {
      entryCount <- Try(Shorts.fromByteArray(bytes))
      parsed <- Try(List.iterate(DataEntry.parse(bytes, 2), entryCount){ case (e, p) => DataEntry.parse(bytes, p) })
      entries = parsed.map(_._1)
      offset = parsed.lastOption.map(_._2) getOrElse 2
      payload <- ValidateModern
          .dataPL(entries)
          .fold(
            errs => Failure(new Exception(errs.toString())),
            pl => Success(pl)
          )
    } yield (payload, offset)
  }
}
