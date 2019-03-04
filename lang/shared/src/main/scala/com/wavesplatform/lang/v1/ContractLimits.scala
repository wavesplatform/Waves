package com.wavesplatform.lang.v1

object ContractLimits {
  val MaxExprComplexity  = 20 * 100
  val MaxExprSizeInBytes = 8 * 1024

  val MaxContractComplexity  = 20 * 100
  val MaxContractSizeInBytes = 32 * 1024

  // As in Scala
  val MaxContractInvocationArgs = 22

  // Data	0.001 per kilobyte, rounded up, fee for CI is 0.005
  val MaxContractInvocationSizeInBytes = 5 * 1024
  val MaxWriteSetSizeInBytes           = 5 * 1024

  // Mass Transfer	0.001 + 0.0005*N, rounded up to 0.001, fee for CI is 0.005
  val MaxPaymentAmount = 10
}
