package com.wavesplatform.api.http.assets

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

import scala.annotation.meta.field

@ApiModel
case class TransferV2Request(
    @(ApiModelProperty @field)(
      dataType = "string",
      example = "3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc",
      required = false
    ) assetId: Option[String],
    @(ApiModelProperty @field)(dataType = "long", example = "300000000") amount: Long,
    @(ApiModelProperty @field)(
      dataType = "string",
      example = "3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc",
      required = false
    ) feeAssetId: Option[String],
    @(ApiModelProperty @field)(dataType = "integer", example = "100000") fee: Long,
    @(ApiModelProperty @field)(dataType = "string", example = "3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef") sender: String,
    @(ApiModelProperty @field)(dataType = "string", example = "Thank you for your kindness", required = false) attachment: Option[String],
    @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk") recipient: String,
    @(ApiModelProperty @field)(dataType = "long", example = "1533832573000", required = false) timestamp: Option[Long] = None)

object TransferV2Request {
  implicit val format: Format[TransferV2Request] = Json.format
}
