package com.wavesplatform.settings

case class CorsHeaders(
    accessControlAllowHeaders: Seq[String],
    accessControlAllowOrigin: Option[String],
    accessControlAllowMethods: Seq[String],
    accessControlAllowCredentials: Boolean
)
