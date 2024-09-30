package com.wavesplatform.api.http

import cats.Applicative
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.{DigestLength, SignatureLength}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, Validation}
import com.wavesplatform.transaction.{Asset, AssetIdStringLength, Proofs, TxValidationError, TxVersion}
import com.wavesplatform.utils.base58Length
import play.api.libs.json.*
import supertagged.TaggedType

package object requests {
  import cats.instances.list.*
  import cats.syntax.either.*
  import cats.syntax.traverse.*

  val SignatureStringLength: Int = base58Length(SignatureLength)
  val DigestStringLength: Int    = base58Length(DigestLength)

  def parseBase58(v: String, error: String, maxLength: Int): Validation[ByteStr] =
    if (v.length > maxLength) Left(TxValidationError.GenericError(error))
    else ByteStr.decodeBase58(v).toOption.toRight(TxValidationError.GenericError(error))

  def parseBase58(v: Option[String], error: String, maxLength: Int): Validation[ByteStr] =
    v.fold[Either[ValidationError, ByteStr]](Right(ByteStr(Array.emptyByteArray)))(_v => parseBase58(_v, error, maxLength))

  def parseBase58ToOption(v: Option[String], error: String, maxLength: Int): Validation[Option[ByteStr]] =
    v.fold[Either[ValidationError, Option[ByteStr]]](Right(None)) { s =>
      parseBase58(s, error, maxLength).map(b => Option(b))
    }

  def parseBase58ToIssuedAsset(v: String): Validation[IssuedAsset] =
    parseBase58(v, "invalid.assetId", AssetIdStringLength)
      .map(IssuedAsset(_))

  def parseBase58ToAsset(v: Option[String], err: String): Validation[Asset] =
    parseBase58ToOption(v.filter(_.length > 0), err, AssetIdStringLength)
      .map {
        case Some(str) => IssuedAsset(str)
        case None      => Waves
      }

  def toProofs(maybeSignature: Option[ByteStr], maybeProofs: Option[Proofs]): Validation[Proofs] =
    (maybeSignature, maybeProofs) match {
      case (Some(sig), Some(proofs)) if proofs.nonEmpty && proofs.head != sig =>
        Left(GenericError("Both proofs and signature are provided, but proofs do not match signature"))
      case _ =>
        maybeProofs
          .orElse(maybeSignature.map(s => Proofs(List(s))))
          .fold[Either[ValidationError, Proofs]](Proofs.empty.asRight)(p => Proofs.create(p))
    }

  implicit val jsResultApplicative: Applicative[JsResult] = new Applicative[JsResult] {
    override def pure[A](x: A): JsResult[A] = JsSuccess(x)

    override def ap[A, B](ff: JsResult[A => B])(fa: JsResult[A]): JsResult[B] = (ff, fa) match {
      case (JsSuccess(f, _), JsSuccess(a, _)) => JsSuccess(f(a))
      case (JsError(e1), JsError(e2))         => JsError(JsError.merge(e1, e2))
      case (JsError(e), _)                    => JsError(e)
      case (_, JsError(e))                    => JsError(e)
    }
  }

  implicit val proofsReads: Reads[Proofs] = Reads {
    case JsArray(values) =>
      values.toList
        .traverse {
          case JsString(v) =>
            JsSuccess(v).flatMap(s => ByteStr.decodeBase58(s).fold(e => JsError(JsonValidationError("invalid.base58", e.getMessage)), JsSuccess(_)))
          case _ => JsError("expected.string")
        }
        .flatMap(Proofs.create(_) match {
          case Right(value) => JsSuccess(value)
          case Left(err)    => JsError(JsonValidationError("invalid.proofs", err.toString))
        })
    case JsNull => JsSuccess(Proofs.empty)
    case _      => JsError("invalid.proofs")
  }

  implicit val proofsWrites: Writes[Proofs] = Writes { proofs =>
    JsArray(proofs.map(s => JsString(s.toString)))
  }

  implicit val byteStrFormat: Format[ByteStr] = com.wavesplatform.utils.byteStrFormat

  object ProofStr extends TaggedType[String]
  type ProofStr = ProofStr.Type

  implicit object ProofStrReads extends Reads[ProofStr] {
    override def reads(json: JsValue): JsResult[ProofStr] = json match {
      case JsNull      => JsSuccess(ProofStr(""))
      case JsString(s) => JsSuccess(ProofStr(s))
      case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsstring"))))
    }
  }

  private[requests] def defaultVersion   = TxVersion.V1
  private[requests] def defaultTimestamp = 0L
}
