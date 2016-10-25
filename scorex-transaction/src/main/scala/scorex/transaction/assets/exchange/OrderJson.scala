package scorex.transaction.assets.exchange

import play.api.data.validation.ValidationError
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success}

object OrderJson {

  import play.api.libs.functional.syntax._
  import play.api.libs.json.Reads._

  implicit val byteArrayReads = new Reads[Array[Byte]] {
    def reads(json: JsValue) = json match {
      case JsString(s) => Base58.decode(s) match {
        case Success(bytes) => JsSuccess(bytes)
        case Failure(_) => JsError(Seq(JsPath() -> Seq(ValidationError("error.incorrect.base58"))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
  }

  implicit val publicKeyAccountReads = new Reads[PublicKeyAccount] {
    def reads(json: JsValue) = json match {
      case JsString(s) => Base58.decode(s) match {
        case Success(bytes) if bytes.length == 32 => JsSuccess(new PublicKeyAccount(bytes))
        case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.incorrect.publicKeyAccount"))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
  }

  def readOrder(sender: PublicKeyAccount, matcher: PublicKeyAccount, spendAssetID: Array[Byte],
                        receiveAssetID: Array[Byte], price: Long, amount: Long, maxTime: Long, matcherFee: Long,
                        signature: Option[Array[Byte]]): Order = {
    Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, signature.getOrElse(Array()))
  }

  implicit val orderReads: Reads[Order] = {
    val r = (JsPath \ "sender").read[PublicKeyAccount] and
      (JsPath \ "matcher").read[PublicKeyAccount] and
      (JsPath \ "spendAssetId").read[Array[Byte]] and
      (JsPath \ "receiveAssetId").read[Array[Byte]] and
      (JsPath \ "price").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "maxTimestamp").read[Long] and
      (JsPath \ "matcherFee").read[Long] and
      (JsPath \ "signature").readNullable[Array[Byte]]
    r(readOrder _)
  }

  /*implicit val orderJsonWrites: Writes[IssueRequest] = (
    (JsPath \ "sender").write[String] and
      (JsPath \ "assetIdOpt").writeNullable[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "quantity").write[Long] and
      (JsPath \ "decimals").write[Byte] and
      (JsPath \ "reissuable").write[Boolean] and
      (JsPath \ "fee").write[Long]
    ) (unlift(IssueRequest.unapply))


  implicit val paymentReads: Reads[IssueRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "assetIdOpt").readNullable[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "decimals").read[Byte] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long]
    ) (IssueRequest.apply _)*/
}