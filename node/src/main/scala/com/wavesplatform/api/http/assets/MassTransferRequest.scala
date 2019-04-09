package com.wavesplatform.api.http.assets

import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

import scala.annotation.meta.field

@ApiModel
case class MassTransferRequest(
    @(ApiModelProperty @field)(
      dataType = "string",
      example = "3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc",
      required = false
    ) assetId: Option[String],
    @(ApiModelProperty @field)(dataType = "string", example = "3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef", required = true) sender: String,
    @(ApiModelProperty @field)(required = true) transfers: List[Transfer],
    @(ApiModelProperty @field)(dataType = "integer", example = "100000", required = true) fee: Long,
    @(ApiModelProperty @field)(dataType = "string", example = "Thank you for your kindness", required = false) attachment: Option[String],
    @(ApiModelProperty @field)(dataType = "long", example = "1533832573000", required = false) timestamp: Option[Long] = None)

object MassTransferRequest {
  implicit val reads = Json.reads[MassTransferRequest]
}
