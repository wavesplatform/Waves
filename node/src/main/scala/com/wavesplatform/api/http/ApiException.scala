package com.wavesplatform.api.http

case class ApiException(apiError: ApiError) extends Exception(apiError.message)
