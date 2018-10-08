package com.wavesplatform.api.http

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

import scala.annotation.meta.field

@ApiModel
case class ConnectReq(@(ApiModelProperty @field)(
                        dataType = "string",
                        example = "127.0.0.1",
                        required = true
                      ) host: String,
                      @(ApiModelProperty @field)(
                        dataType = "integer",
                        example = "6868",
                        required = true
                      ) port: Int)

object ConnectReq {
  implicit val connectFormat: Format[ConnectReq] = Json.format
}
