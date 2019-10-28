package com.wavesplatform

import java.nio.charset.StandardCharsets

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, Proofs}
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.reflect.ClassTag
import scala.util.{Failure, Success}

package object http {

  val Waves: Long = 100000000L

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

      case _ => JsError("Can't read PublicKey")
    },
    Writes(x => JsString(x.toString))
  )

  implicit val PublicKeyFormat: Format[PublicKey] = byteStrFormat.inmap[PublicKey](
    x => PublicKey(x.arr),
    x => ByteStr(x)
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
          .tryDecodeWithLimit(str)
          .toEither
          .flatMap(AddressOrAlias.fromBytes(_, 0))
          .map { case (x, _) => JsSuccess(x) }
          .getOrElse(JsError("Can't read PublicKey"))

      case _ => JsError("Can't read PublicKey")
    },
    Writes(x => JsString(x.bytes.toString))
  )

  implicit val versionedTransferTransactionFormat: Reads[TransferTransaction] = (
    (JsPath \ "version").readNullable[Byte] and
      (JsPath \ "sender").read[PublicKey] and
      (JsPath \ "recipient").read[AddressOrAlias] and
      (JsPath \ "assetId").read[Asset] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "feeAssetId").read[Asset] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "attachment").read[String].map[Array[Byte]](_.getBytes(StandardCharsets.UTF_8)) and
      (JsPath \ "proofs").readNullable[Proofs] and
      (JsPath \ "signature").readNullable[ByteStr]
  ) { (version, sender, recipient, asset, amount, timestamp, feeAsset, fee, attachment, proofs, signature) =>
    TransferTransaction.create(version.getOrElse(1.toByte), timestamp, sender, recipient, asset, amount, feeAsset, fee, attachment, proofs.orElse(signature.map(s => Proofs(Seq(s)))).get).explicitGet()
  }
}
