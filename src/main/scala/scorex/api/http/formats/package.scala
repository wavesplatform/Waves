package scorex.api.http

import scala.util.{Failure, Success}
import play.api.libs.json.{Format, JsError, JsResult, JsString, JsSuccess, JsValue, JsonValidationError, Reads}
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.TypedTransaction.{KeyLength, SignatureLength}

package object formats {
  implicit val AccountFormat: Format[Account] = Format(
    (json: JsValue) => json.validate[String] flatMap { address =>
      if (Account.isValidAddress(address)) {
        JsSuccess(new Account(address))
      } else {
        JsError(s"Invalid address '$address'")
      }
    },
    (a: Account) => JsString(a.address)
  )

  private class Base58Reads[A](requiredLength: Int, f: Array[Byte] => A) extends Reads[A] {
    override def reads(json: JsValue): JsResult[A] = json.validate[String] flatMap { base58String =>
      Base58.decode(base58String) match {
        case Success(bytes) =>
          if (bytes.length == requiredLength) {
            JsSuccess(f(bytes))
          } else {
            JsError(JsonValidationError("error.invalid.length"))
          }
        case Failure(_) =>
          JsError(JsonValidationError("error.invalid.base58"))
      }
    }
  }

  implicit val PublicKeyAccountFormat: Format[PublicKeyAccount] = Format(
    new Base58Reads(KeyLength, new PublicKeyAccount(_)),
    (o: PublicKeyAccount) => JsString(Base58.encode(o.publicKey))
  )

  val SignatureFormat: Format[String] = Format(
    new Base58Reads(SignatureLength, Base58.encode),
    (v: String) => JsString(v)
  )
}
