package com.wavesplatform.transaction.smart.script
import com.wavesplatform.lang.StdLibVersion.{V1, V3}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.FunctionIds.SIGVERIFY
import com.wavesplatform.utils.functionCosts

object ContractLimits {
  val MaxExprComplexity  = 20 * functionCosts(V1)(FunctionHeader.Native(SIGVERIFY))()
  val MaxExprSizeInBytes = 8 * 1024

  val MaxContractComplexity  = 20 * functionCosts(V3)(FunctionHeader.Native(SIGVERIFY))()
  val MaxContractSizeInBytes = 32 * 1024

}
