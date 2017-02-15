package scorex.api.http

import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TypedTransaction._
import scala.util.{Failure, Success}

object formats {

  object AccountReads extends Reads[Account] {
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

  object AccountWrites extends Writes[Account] {
    override def writes(o: Account): JsValue = JsString(o.address)
  }

  implicit val accountFormat = Format(AccountReads, AccountWrites)

  object PublicKeyAccountReads extends Reads[PublicKeyAccount] {
    override def reads(json: JsValue): JsResult[PublicKeyAccount] = {
      json.validate[String].flatMap(base58String => {
        val bytesT = Base58.decode(base58String)
        bytesT match {
          case Success(bytes) =>
            if (bytes.length == KeyLength) {
              JsSuccess(new PublicKeyAccount(bytes))
            } else {
              JsError(s"Invalid length of public key '$base58String': ${base58String.length} != ${KeyLength}")
            }
          case Failure(f) =>
            JsError(s"Invalid base58 string in public key '$base58String'")
        }
      })
    }
  }

  object PublicKeyAccountWrites extends Writes[PublicKeyAccount] {
    override def writes(o: PublicKeyAccount): JsValue = JsString(Base58.encode(o.publicKey))
  }

  implicit val publicKeyAccountFormat = Format(PublicKeyAccountReads, PublicKeyAccountWrites)

  object SignatureReads extends Reads[String] {
    override def reads(json: JsValue): JsResult[String] = {
      json.validate[String].flatMap(base58String => {
        val bytesT = Base58.decode(base58String)
        bytesT match {
          case Success(bytes) =>
            if (bytes.length == SignatureLength) {
              JsSuccess(Base58.encode(bytes))
            } else {
              JsError(s"Invalid length of signature '$base58String': ${base58String.length} != ${SignatureLength}")
            }
          case Failure(f) =>
            JsError(s"Invalid base58 string in signature '$base58String'")
        }
      })
    }
  }

  implicit val signatureFormat = Format(SignatureReads, ???)

}
