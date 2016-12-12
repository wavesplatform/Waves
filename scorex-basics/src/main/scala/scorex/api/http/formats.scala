package scorex.api.http

import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success}

object formats {

  implicit object AccountReads extends Reads[Account] {
    override def reads(json: JsValue): JsResult[Account] = {
      json.validate[String].flatMap(address => {
        if (address.length == 35) {
          JsSuccess(new Account(address))
        } else {
          JsError(s"Invalid length of address '$address': ${address.length} != 35")
        }
      })
    }
  }

  implicit object AccountWrites extends Writes[Account] {
    override def writes(o: Account): JsValue = JsString(o.address)
  }

  implicit object PublicKeyAccountReads extends Reads[PublicKeyAccount] {
    override def reads(json: JsValue): JsResult[PublicKeyAccount] = {
      json.validate[String].flatMap(base58String => {
        val bytesT = Base58.decode(base58String)
        bytesT match {
          case Success(bytes) =>
            if (bytes.length == EllipticCurveImpl.KeyLength) {
              JsSuccess(new PublicKeyAccount(bytes))
            } else {
              JsError(s"Invalid length of public key '$base58String': ${base58String.length} != ${EllipticCurveImpl.KeyLength}")
            }
          case Failure(f) =>
            JsError(s"Invalid base58 string in public key '$base58String'")
        }
      })
    }
  }

  implicit object PublicKeyAccountWrites extends Writes[PublicKeyAccount] {
    override def writes(o: PublicKeyAccount): JsValue = JsString(Base58.encode(o.publicKey))
  }

  object SignatureReads extends Reads[String] {
    override def reads(json: JsValue): JsResult[String] = {
      json.validate[String].flatMap(base58String => {
        val bytesT = Base58.decode(base58String)
        bytesT match {
          case Success(bytes) =>
            if (bytes.length == EllipticCurveImpl.SignatureLength) {
              JsSuccess(Base58.encode(bytes))
            } else {
              JsError(s"Invalid length of signature '$base58String': ${base58String.length} != ${EllipticCurveImpl.SignatureLength}")
            }
          case Failure(f) =>
            JsError("Invalid base58 string in signature '$base58String'")
        }
      })
    }
  }

}
