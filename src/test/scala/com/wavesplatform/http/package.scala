package com.wavesplatform

import java.nio.charset.StandardCharsets

import com.wavesplatform.state.ByteStr
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.{AssetId, Proofs}
import scorex.transaction.assets.{TransferTransaction, VersionedTransferTransaction}
import shapeless.{:+:, CNil, Coproduct}

import scala.reflect.ClassTag
import scala.util.{Failure, Success}

package object http {

  val Waves: Long  = 100000000L
  val ApiKeyHeader = api_key("ridethewaves!")

  def sameSignature(target: Array[Byte])(actual: Array[Byte]): Boolean = target sameElements actual

  implicit class JsFieldTypeChecker(val s: String) extends AnyVal {
    def ofType[A <: JsValue](implicit r: Reads[A], t: ClassTag[A]) = HavePropertyMatcher[JsValue, Any] { json =>
      val actualValue = (json \ s).validate[A]
      HavePropertyMatchResult(actualValue.isSuccess, s, t.runtimeClass.getSimpleName, (json \ s).get)
    }
  }

  implicit def tuple2ToHPM(v: (String, JsValue)): HavePropertyMatcher[JsValue, JsValue] =
    HavePropertyMatcher[JsValue, JsValue] { json =>
      val actualFieldValue = (json \ v._1).as[JsValue]
      HavePropertyMatchResult(actualFieldValue == v._2, v._1, v._2, actualFieldValue)
    }

  implicit val byteStrFormat: Format[ByteStr] = Format(
    Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read PublicKeyAccount")
    },
    Writes(x => JsString(x.base58))
  )

  implicit val publicKeyAccountFormat: Format[PublicKeyAccount] = byteStrFormat.inmap[PublicKeyAccount](
    x => PublicKeyAccount(x.arr),
    x => ByteStr(x.publicKey)
  )

  implicit val proofsFormat: Format[Proofs] = Format(
    Reads {
      case JsArray(xs) =>
        xs.foldLeft[JsResult[Proofs]](JsSuccess(Proofs.empty)) {
          case (r: JsError, _) => r
          case (JsSuccess(r, _), JsString(rawProof)) =>
            ByteStr.decodeBase58(rawProof) match {
              case Failure(e) => JsError(e.toString)
              case Success(x) => JsSuccess(Proofs(r.proofs :+ x))
            }
          case _ => JsError("Can't parse proofs")
        }
      case _ => JsError("Can't parse proofs")
    },
    Writes { proofs =>
      JsArray(proofs.proofs.map(byteStrFormat.writes))
    }
  )

  implicit val addressOrAliasFormat: Format[AddressOrAlias] = Format[AddressOrAlias](
    Reads {
      case JsString(str) =>
        Base58
          .decode(str)
          .toEither
          .flatMap(AddressOrAlias.fromBytes(_, 0))
          .map { case (x, _) => JsSuccess(x) }
          .getOrElse(JsError("Can't read PublicKeyAccount"))

      case _ => JsError("Can't read PublicKeyAccount")
    },
    Writes(x => JsString(x.bytes.base58))
  )

  implicit val transferTransactionFormat: Format[TransferTransaction] = (
    (JsPath \ "assetId").formatNullable[AssetId] and
      (JsPath \ "sender").format[PublicKeyAccount] and
      (JsPath \ "recipient").format[AddressOrAlias] and
      (JsPath \ "amount").format[Long] and
      (JsPath \ "timestamp").format[Long] and
      (JsPath \ "feeAsset").formatNullable[AssetId] and
      (JsPath \ "fee").format[Long] and
      (JsPath \ "attachment")
        .format[String]
        .inmap[Array[Byte]](
          _.getBytes(StandardCharsets.UTF_8),
          xs => new String(xs, StandardCharsets.UTF_8)
        ) and
      (JsPath \ "signature").format[ByteStr]
  )(TransferTransaction.apply, unlift(TransferTransaction.unapply))

  implicit val versionedTransferTransactionFormat: Format[VersionedTransferTransaction] = (
    (JsPath \ "version").format[Byte] and
      (JsPath \ "sender").format[PublicKeyAccount] and
      (JsPath \ "recipient").format[AddressOrAlias] and
      (JsPath \ "assetId").formatNullable[AssetId] and
      (JsPath \ "amount").format[Long] and
      (JsPath \ "timestamp").format[Long] and
      (JsPath \ "feeAssetId").formatNullable[AssetId] and
      (JsPath \ "fee").format[Long] and
      (JsPath \ "attachment")
        .format[String]
        .inmap[Array[Byte]](
          _.getBytes(StandardCharsets.UTF_8),
          xs => new String(xs, StandardCharsets.UTF_8)
        ) and
      (JsPath \ "proofs").format[Proofs]
  )(VersionedTransferTransaction.apply, unlift(VersionedTransferTransaction.unapply))

  type TransferTransactions = TransferTransaction :+: VersionedTransferTransaction :+: CNil
  implicit val autoTransferTransactionsReads: Reads[TransferTransactions] = Reads { json =>
    (json \ "version").asOpt[Byte] match {
      case None => transferTransactionFormat.reads(json).map(Coproduct[TransferTransactions](_))
      case _    => versionedTransferTransactionFormat.reads(json).map(Coproduct[TransferTransactions](_))
    }
  }
}
