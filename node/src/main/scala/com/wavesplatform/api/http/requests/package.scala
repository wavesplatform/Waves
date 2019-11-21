package com.wavesplatform.api.http

import java.nio.charset.StandardCharsets

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.{DigestLength, SignatureLength}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, Validation}
import com.wavesplatform.transaction.{Asset, AssetIdStringLength, Proofs, TxValidationError, TxVersion}
import com.wavesplatform.utils.base58Length
import play.api.libs.json._
import supertagged.TaggedType

package object requests {
  import cats.instances.either._
  import cats.instances.list._
  import cats.syntax.either._
  import cats.syntax.traverse._

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

  def parseBase58ToAsset(v: String): Validation[IssuedAsset] =
    parseBase58(v, "invalid.assetId", AssetIdStringLength)
      .map(IssuedAsset)

  def parseBase58ToAssetId(v: Option[String], err: String): Validation[Asset] =
    parseBase58ToOption(v.filter(_.length > 0), err, AssetIdStringLength)
      .map {
        case Some(str) => IssuedAsset(str)
        case None      => Waves
      }

  def toAsset(maybeAsset: Option[String]): Either[ValidationError, Asset] =
    maybeAsset match {
      case Some(v) if v.nonEmpty => ByteStr.decodeBase58(v).toEither.leftMap(e => GenericError(e.getMessage)).map(IssuedAsset)
      case None                  => Waves.asRight
      case _                     => GenericError("requirement failed: empty string").asLeft
    }

  def toAttachment(maybeAttachment: Option[String]): Either[ValidationError, Array[Byte]] =
    maybeAttachment match {
      case Some(v) if v.nonEmpty => Base58.tryDecodeWithLimit(v).toEither.leftMap(e => GenericError(e.getMessage))
      case _                     => Array.emptyByteArray.asRight
    }

  def toProofs(version: Option[Byte], maybeSignature: Option[String], maybeProofs: Option[List[String]]): Either[ValidationError, Proofs] =
    version match {
      case Some(v) if v == 2.toByte =>
        maybeProofs match {
          case Some(proofs) =>
            for {
              proofsBytes <- proofs.traverse(s => parseBase58(s, "invalid.proofs", Proofs.MaxProofStringSize))
              result      <- Proofs.create(proofsBytes)
            } yield result
          case None => Proofs.empty.asRight
        }
      case _ =>
        maybeSignature match {
          case Some(str) => parseBase58(str, "invalid.signature", SignatureStringLength).map(sig => Proofs(sig))
          case None      => Proofs.empty.asRight
        }
    }

  object ProofStr extends TaggedType[String]
  type ProofStr = ProofStr.Type

  implicit object ProofStrReads extends Reads[ProofStr] {
    override def reads(json: JsValue): JsResult[ProofStr] = json match {
      case JsNull      => JsSuccess(ProofStr(""))
      case JsString(s) => JsSuccess(ProofStr(s))
      case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsstring"))))
    }
  }

  private[requests] def defaultVersion = TxVersion.V1
  private[requests] def defaultTimestamp = 0L
}
