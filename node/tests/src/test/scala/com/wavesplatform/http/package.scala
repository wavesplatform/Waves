package com.wavesplatform

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, Proofs}
import org.scalatest.enablers.Emptiness
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.{Failure, Success}

package object http {
  def sameSignature(target: Array[Byte])(actual: Array[Byte]): Boolean = target sameElements actual

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
    x => ByteStr(x.arr)
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
          .flatMap[Object, AddressOrAlias](AddressOrAlias.fromBytes)
          .map(JsSuccess(_))
          .getOrElse(JsError("Can't read PublicKey"))

      case _ => JsError("Can't read PublicKey")
    },
    Writes(x => JsString(x.toString))
  )

  implicit val versionedTransferTransactionFormat: Reads[TransferTransaction] = (
    (JsPath \ "version").readNullable[Byte] and
      (JsPath \ "senderPublicKey").read[PublicKey] and
      (JsPath \ "recipient").read[AddressOrAlias] and
      (JsPath \ "assetId").read[Asset] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "feeAssetId").read[Asset] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "attachment").readWithDefault(ByteStr.empty) and
      (JsPath \ "proofs").readNullable[Proofs] and
      (JsPath \ "signature").readNullable[ByteStr]
  ) { (version, sender, recipient, asset, amount, timestamp, feeAsset, fee, attachment, proofs, signature) =>
    TransferTransaction
      .create(
        version.getOrElse(1.toByte),
        sender,
        recipient,
        asset,
        amount,
        feeAsset,
        fee,
        attachment,
        timestamp,
        proofs.orElse(signature.map(s => Proofs(Seq(s)))).get
      )
      .explicitGet()
  }

  implicit val emptyJsLookupResult: Emptiness[JsLookupResult] = r => r.isEmpty
}
