package scorex.api.http

import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success}

object reads {

  implicit object AccountReads extends Reads[Account] {
    override def reads(json: JsValue): JsResult[Account] = {
      json.validate[String].flatMap(address => {
        if (address.length == Account.AddressLength) {
          JsSuccess(new Account(address))
        } else {
          JsError(s"Invalid length of address '$address': ${address.length} != ${Account.AddressLength}")
        }
      })
    }
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

  object SignatureReads extends Reads[Array[Byte]] {
    override def reads(json: JsValue): JsResult[Array[Byte]] = {
      json.validate[String].flatMap(base58String => {
        val bytesT = Base58.decode(base58String)
        bytesT match {
          case Success(bytes) =>
            if (bytes.length == EllipticCurveImpl.SignatureLength) {
              JsSuccess(bytes)
            } else {
              JsError(s"Invalid length of signature '$base58String': ${base58String.length} != ${EllipticCurveImpl.SignatureLength}")
            }
          case Failure(f) =>
            JsError(s"Invalid base58 string in signature '$base58String'")
        }
      })
    }
  }

}
