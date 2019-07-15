package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{Asset, Proofs}
import play.api.libs.json._

import scala.util.{Failure, Success}

object OrderJson {
  import play.api.libs.functional.syntax._
  import play.api.libs.json.Reads._

  implicit val byteArrayReads: Reads[Array[Byte]] = {
    case JsString(s) =>
      Base58.tryDecodeWithLimit(s) match {
        case Success(bytes) => JsSuccess(bytes)
        case Failure(_)     => JsError(JsPath, JsonValidationError("error.incorrect.base58"))
      }
    case _ => JsError(JsPath, JsonValidationError("error.expected.jsstring"))
  }

  implicit val optionByteArrayReads: Reads[Option[Array[Byte]]] = {
    case JsString(s) if s.isEmpty => JsSuccess(Option.empty[Array[Byte]])
    case JsString(s) if s.nonEmpty =>
      Base58.tryDecodeWithLimit(s) match {
        case Success(bytes) => JsSuccess(Some(bytes))
        case Failure(_)     => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.incorrect.base58"))))
      }
    case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsstring"))))
  }

  implicit lazy val accountPublicKeyReads: Reads[PublicKey] = Reads {
    case JsString(s) =>
      Base58.tryDecodeWithLimit(s) match {
        case Success(bytes) if bytes.length == 32 => JsSuccess(PublicKey(bytes))
        case _                                    => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.incorrectAccount"))))
      }
    case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsstring"))))
  }

  def readOrderV1V2(sender: PublicKey,
                    matcher: PublicKey,
                    assetPair: AssetPair,
                    orderType: OrderType,
                    amount: Long,
                    price: Long,
                    timestamp: Long,
                    expiration: Long,
                    matcherFee: Long,
                    signature: Option[Array[Byte]],
                    proofs: Option[Array[Array[Byte]]],
                    version: Option[Byte]): Order = {

    val eproofs =
      proofs
        .map(p => Proofs(p.map(ByteStr.apply).toList))
        .orElse(signature.map(s => Proofs(List(ByteStr(s)))))
        .getOrElse(Proofs.empty)

    val vrsn: Byte = version.getOrElse(if (eproofs.proofs.size == 1 && eproofs.proofs.head.arr.length == SignatureLength) 1 else 2)
    Order(
      sender,
      matcher,
      assetPair,
      orderType,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      eproofs,
      vrsn
    )
  }

  def readOrderV3(sender: PublicKey,
                  matcher: PublicKey,
                  assetPair: AssetPair,
                  orderType: OrderType,
                  amount: Long,
                  price: Long,
                  timestamp: Long,
                  expiration: Long,
                  matcherFee: Long,
                  signature: Option[Array[Byte]],
                  proofs: Option[Array[Array[Byte]]],
                  version: Byte,
                  matcherFeeAssetId: Asset): Order = {

    val eproofs =
      proofs
        .map(p => Proofs(p.map(ByteStr.apply)))
        .orElse(signature.map(s => Proofs(Seq(ByteStr(s)))))
        .getOrElse(Proofs.empty)

    Order(
      sender,
      matcher,
      assetPair,
      orderType,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      eproofs,
      version,
      matcherFeeAssetId
    )
  }

  private val assetReads: Reads[Asset] = {
    case JsNull | JsString("") => JsSuccess(Waves)
    case JsString(s) =>
      AssetPair.extractAssetId(s) match {
        case Failure(_)       => JsError(JsPath, JsonValidationError("error.incorrect.base58"))
        case Success(assetId) => JsSuccess(assetId)
      }
    case _ => JsError(JsPath, JsonValidationError("error.expected.jsstring"))
  }

  implicit val assetPairReads: Reads[AssetPair] = {
    val r = (JsPath \ "amountAsset").readWithDefault[Asset](Waves)(assetReads) and
      (JsPath \ "priceAsset").readWithDefault[Asset](Waves)(assetReads)
    r(AssetPair(_, _))
  }

  implicit val orderTypeReads: Reads[OrderType] =
    JsPath.read[String].map(OrderType.apply)

  private val orderV1V2Reads: Reads[Order] = {
    val r = (JsPath \ "senderPublicKey").read[PublicKey](accountPublicKeyReads) and
      (JsPath \ "matcherPublicKey").read[PublicKey](accountPublicKeyReads) and
      (JsPath \ "assetPair").read[AssetPair] and
      (JsPath \ "orderType").read[OrderType] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "price").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "expiration").read[Long] and
      (JsPath \ "matcherFee").read[Long] and
      (JsPath \ "signature").readNullable[Array[Byte]] and
      (JsPath \ "proofs").readNullable[Array[Array[Byte]]] and
      (JsPath \ "version").readNullable[Byte]
    r(readOrderV1V2 _)
  }

  private val orderV3Reads: Reads[Order] = {
    val r = (JsPath \ "senderPublicKey").read[PublicKey](accountPublicKeyReads) and
      (JsPath \ "matcherPublicKey").read[PublicKey](accountPublicKeyReads) and
      (JsPath \ "assetPair").read[AssetPair] and
      (JsPath \ "orderType").read[OrderType] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "price").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "expiration").read[Long] and
      (JsPath \ "matcherFee").read[Long] and
      (JsPath \ "signature").readNullable[Array[Byte]] and
      (JsPath \ "proofs").readNullable[Array[Array[Byte]]] and
      (JsPath \ "version").read[Byte] and
      (JsPath \ "matcherFeeAssetId")
        .readNullable[Array[Byte]]
        .map(arrOpt => Asset.fromCompatId(arrOpt.map(ByteStr(_))))
    r(readOrderV3 _)
  }

  implicit val orderReads: Reads[Order] = {
    case jsOrder @ JsObject(map) =>
      map.getOrElse("version", JsNumber(1)) match {
        case JsNumber(x) if x.byteValue() == 3 => orderV3Reads.reads(jsOrder)
        case _                                 => orderV1V2Reads.reads(jsOrder)
      }
    case invalidOrder => JsError(s"Can't parse invalid order $invalidOrder")
  }

  implicit val orderFormat: Format[Order] = Format(orderReads, Writes[Order](_.json()))

}
