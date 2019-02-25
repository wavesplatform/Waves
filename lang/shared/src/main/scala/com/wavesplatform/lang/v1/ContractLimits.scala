package com.wavesplatform.lang.v1

object ContractLimits {
  val MaxExprComplexity  = 20 * 100
  val MaxExprSizeInBytes = 8 * 1024

  val MaxContractComplexity            = 20 * 100
  val MaxContractSizeInBytes           = 32 * 1024
  val MaxContractInvocationArgs        = 22
  val MaxContractInvocationSizeInBytes = 5 * 1024
  val MaxWriteSetSizeInBytes           = 5 * 1024
}
