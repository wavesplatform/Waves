package com.wavesplatform.it

import java.nio.charset.StandardCharsets

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.{Paged, ScorexLogging}
import org.asynchttpclient.Response
import play.api.libs.functional.syntax._
import play.api.libs.json.Json.parse
import play.api.libs.json.{JsError, JsString, JsSuccess, Reads, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

package object api {
  implicit class ResponseFutureExt(f: Future[Response]) extends ScorexLogging {
    import cats.instances.either._
    import cats.instances.list._
    import cats.syntax.alternative._
    import cats.syntax.either._
    def as[A: Reads](implicit ec: ExecutionContext): Future[A] =
      f.map { r =>
        val json = r.getResponseBody(StandardCharsets.UTF_8)
        Try(parse(json).as[A]).fold(err => throw new RuntimeException(s"Json parse failed: $json", err), identity)
      }(ec)

    def as[A: Reads](numberAsString: Boolean = false)(implicit ec: ExecutionContext): Future[A] = {
      def convert(jsv: JsValue): Either[RuntimeException, JsValue] = {
        val fieldNamesToTranslate = Set(
          "amount",
          "available",
          "balance",
          "buyMatcherFee",
          "currentReward",
          "desiredReward",
          "effective",
          "fee",
          "feeAmount",
          "generating",
          "in",
          "matcherFee",
          "minIncrement",
          "minSponsoredAssetFee",
          "out",
          "price",
          "quantity",
          "regular",
          "reward",
          "sellMatcherFee",
          "sponsorBalance",
          "totalAmount",
          "totalFee",
          "totalWavesAmount",
          "value"
        )
        jsv match {
          case JsArray(arr) =>
            arr.map(convert).toList.separate match {
              case (Nil, elements) => JsArray(elements).asRight
              case (errors, _)     => new RuntimeException(errors.map(_.getMessage).mkString("\n")).asLeft
            }
          case JsObject(srcValues) =>
            val values = srcValues.toList.map {
              case (name, JsString(v)) if fieldNamesToTranslate.contains(name) && v.matches("-?\\d+") =>
                Try(BigDecimal(v)).toEither.map(v => name -> JsNumber(v)).left.map(_ => name -> v)
              case (name, JsNull) if fieldNamesToTranslate.contains(name)      => (name -> JsNull).asRight[String]
              case (name, JsNumber(_)) if fieldNamesToTranslate.contains(name) => name.asLeft[(String, JsValue)]
              case (name, v)                                                   => convert(v).map(r => name -> r)
            }
            values.separate match {
              case (Nil, fields) => JsObject(fields).asRight
              case (errors, _)   => new RuntimeException(s"Invalid number as string: ${errors.mkString("[", ",", "]")}").asLeft
            }
          case jsv => jsv.asRight
        }
      }
      f.map { r =>
        val value  = parse(r.getResponseBody(StandardCharsets.UTF_8))
        val result = if (numberAsString) convert(value) else value.asRight
        result.left.foreach(err => log.error(s"Error converting ${Json.prettyPrint(value)}", err))
        result
      }.flatMap {
        case Right(value) => Future(value.as[A])
        case Left(err)    => Future.failed(err)
      }
    }
  }

  implicit val addressReads: Reads[com.wavesplatform.account.Address] = Reads {
    case JsString(addrStr) =>
      com.wavesplatform.account.Address
        .fromString(addrStr)
        .fold(err => JsError(err.toString), addr => JsSuccess(addr))
    case _ => JsError("Expected base58 encoded address")
  }

  implicit val dstMapReads: Reads[Map[com.wavesplatform.account.Address, Long]] = Reads { json =>
    json.validate[Map[String, Long]].map { dst =>
      dst.map { case (addrStr, balance) =>
        com.wavesplatform.account.Address.fromString(addrStr).explicitGet() -> balance
      }
    }
  }

  implicit val rateMapReads: Reads[Map[Asset, Double]] = Reads { json =>
    json.validate[Map[String, Double]].map { rate =>
      rate.map { case (assetStr, rateValue) => AssetPair.extractAssetId(assetStr).get -> rateValue }
    }
  }

  implicit val distributionReads: Reads[AssetDistribution] = Reads { json =>
    json
      .validate[Map[com.wavesplatform.account.Address, Long]]
      .map(dst => AssetDistribution(dst))
  }

  implicit def pagedReads[C: Reads, R: Reads]: Reads[Paged[C, R]] =
    (
      (JsPath \ "hasNext").read[Boolean] and
        (JsPath \ "lastItem").readNullable[C] and
        (JsPath \ "items").read[R]
    )(Paged.apply[C, R] _)

  implicit val distributionPageReads: Reads[AssetDistributionPage] = Reads { json =>
    json.validate[Paged[com.wavesplatform.account.Address, AssetDistribution]].map(pg => AssetDistributionPage(pg))
  }
}
