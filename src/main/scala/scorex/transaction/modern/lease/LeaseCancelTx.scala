package scorex.transaction.modern.lease

import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.transaction._
import scorex.transaction.base.LeaseCancelTxBase
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.validation.ValidateModern

import scala.util.{Failure, Success, Try}

final case class LeaseCancelPayload(leaseId: ByteStr) extends TxData {
  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(leaseId.arr)

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(Json.obj("leaseId" -> leaseId.base58))
}

final case class LeaseCancelTx(header: TxHeader, payload: LeaseCancelPayload, proofs: Proofs)
    extends ModernTransaction(LeaseCancelTx)
    with LeaseCancelTxBase {
  override val assetFee: (Option[AssetId], Long) = (None, header.fee)

  override val leaseId: AssetId = payload.leaseId
}

object LeaseCancelTx extends TransactionParser.Modern[LeaseCancelTx, LeaseCancelPayload] {
  override val typeId: Byte = 9

  override val supportedVersions: Set[Byte] = Set(3)

  override def create(header: TxHeader, data: LeaseCancelPayload, proofs: Proofs): Try[LeaseCancelTx] =
    Try(LeaseCancelTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(LeaseCancelPayload, Int)] = {
    parseByteStr(bytes.take(crypto.DigestSize))
      .flatMap(lid => {
        ValidateModern
          .leaseCancelPL(lid)
          .fold(
            errs => Failure(new Exception(errs.toString())),
            pl => Success(pl)
          )
      })
      .map(payload => (payload, crypto.DigestSize))
  }
}
