package scorex.transaction.modern

import com.google.common.primitives.{Bytes, Shorts}
import com.wavesplatform.state.DataEntry
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction.{AssetId, Proofs, TransactionParser}

import scala.util.Try

final case class DataPayload(entries: List[DataEntry[_]]) extends TxData {
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Shorts.toByteArray(entries.length.toShort),
      entries.flatMap(_.toBytes).toArray
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj("data"    -> Json.toJson(entries))
  }
}

final case class DataTx(header: TxHeader,
                        payload: DataPayload,
                        proofs: Proofs) extends ModernTransaction(DataTx) {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)
}

object DataTx extends TransactionParser.Modern[DataTx, DataPayload] {
  override val typeId: Byte = 12

  override val supportedVersions: Set[Byte] = Set(2)

  override def create(header: TxHeader, data: DataPayload, proofs: Proofs): Try[DataTx] =
    Try(DataTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(DataPayload, Int)] = Try {
    val entryCount = Shorts.fromByteArray(bytes)
    val (entries, p1) =
      if (entryCount > 0) {
        val parsed = List.iterate(DataEntry.parse(bytes, 2), entryCount) { case (e, p) => DataEntry.parse(bytes, p) }
        (parsed.map(_._1), parsed.last._2)
      } else (List.empty, 2)

    (DataPayload(entries), p1)
  }
}