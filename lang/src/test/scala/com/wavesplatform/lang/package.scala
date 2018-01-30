package com.wavesplatform


package object lang {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
