package scorex.transaction.modern

import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.Alias
import scorex.serialization.Deser
import scorex.transaction._
import scorex.transaction.base.CreateAliasTxBase

import scala.util.{Failure, Success, Try}

final case class CreateAliasPayload(alias: Alias) extends TxData {
  override def bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(Deser.serializeArray(alias.bytes.arr))
  override def json: Coeval[JsObject] =
    Coeval.evalOnce(Json.obj("alias" -> alias.name))
}

final case class CreateAliasTx(header: TxHeader, payload: CreateAliasPayload, proofs: Proofs)
  extends ModernTransaction(CreateAliasTx)
    with CreateAliasTxBase {
  override val assetFee: (Option[AssetId], Long) = (None, header.fee)

  override val alias: Alias = payload.alias
}

object CreateAliasTx extends TransactionParser.Modern[CreateAliasTx, CreateAliasPayload] {
  override val typeId: Byte = 10

  override val supportedVersions: Set[Byte] = Set(2)

  override def create(header: TxHeader, data: CreateAliasPayload, proofs: Proofs): Try[CreateAliasTx] = {
    Success(CreateAliasTx(header, data, proofs))
  }

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(CreateAliasPayload, Int)] = {
    for {
      (aliasBytes, aliasEnd) <- Try(Deser.parseArraySize(bytes, 0))
      alias <- Alias
        .fromBytes(aliasBytes)
        .fold(ve => Failure(new Exception(ve.toString)), Success.apply)
    } yield (CreateAliasPayload(alias), aliasEnd)
  }
}
