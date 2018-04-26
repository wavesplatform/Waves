package scorex.transaction.modern.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction._
import scorex.transaction.base.SponsorFeeTxBase
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.validation.ValidateModern

import scala.util.{Failure, Success, Try}

final case class SponsorFeePayload(assetId: ByteStr, minFee: Long) extends TxData {
  override def bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      assetId.arr,
      Longs.toByteArray(minFee)
    )
  }
  override def json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "assetId" -> assetId.base58,
      "baseFee" -> minFee
    )
  }
}
case class SponsorFeeTx(header: TxHeader, payload: SponsorFeePayload, proofs: Proofs)
    extends ModernTransaction(SponsorFeeTx)
    with SponsorFeeTxBase
    with FastHashId {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)
  override val assetId: AssetId                  = payload.assetId
  override val minFee: Long                      = payload.minFee
}

object SponsorFeeTx extends TransactionParser.Modern[SponsorFeeTx, SponsorFeePayload] {

  override val typeId: Byte                 = 14
  override val supportedVersions: Set[Byte] = Set(1)

  override def create(header: TxHeader, data: SponsorFeePayload, proofs: Proofs): Try[SponsorFeeTx] =
    Try(SponsorFeeTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(SponsorFeePayload, Int)] = {
    for {
      assetId <- parseByteStr(bytes.take(AssetIdLength))
      minFee  <- parseLong(bytes.slice(AssetIdLength, AssetIdLength + 8))
      payload <- ValidateModern
        .sponsorFellPL(assetId, minFee)
        .fold(
          errs => Failure(new Exception(errs.toString())),
          pl => Success(pl)
        )
    } yield (payload, AssetIdLength + 8)
  }
}
