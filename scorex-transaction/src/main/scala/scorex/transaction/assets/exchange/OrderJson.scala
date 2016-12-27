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

  implicit val optionByteArrayReads = new Reads[Option[Array[Byte]]] {
    def reads(json: JsValue) = json match {
      case JsString(s) if s.isEmpty => JsSuccess(Option.empty[Array[Byte]])
      case JsString(s) if s.nonEmpty => Base58.decode(s) match {
        case Success(bytes) => JsSuccess(Some(bytes))
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

  def readOrder(sender: PublicKeyAccount, matcher: PublicKeyAccount, spendAssetID: Option[Option[Array[Byte]]],
                        receiveAssetID: Option[Option[Array[Byte]]], price: Long, amount: Long, maxTime: Long, matcherFee: Long,
                        signature: Option[Array[Byte]]): Order = {
    Order(sender, matcher, spendAssetID.flatten, receiveAssetID.flatten, price, amount, maxTime, matcherFee, signature.getOrElse(Array()))
  }

  implicit val orderReads: Reads[Order] = {
    val r = (JsPath \ "sender").read[PublicKeyAccount] and
      (JsPath \ "matcher").read[PublicKeyAccount] and
      (JsPath \ "spendAssetId").readNullable[Option[Array[Byte]]] and
      (JsPath \ "receiveAssetId").readNullable[Option[Array[Byte]]] and
      (JsPath \ "price").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "maxTimestamp").read[Long] and
      (JsPath \ "matcherFee").read[Long] and
      (JsPath \ "signature").readNullable[Array[Byte]]
    r(readOrder _)
  }

}