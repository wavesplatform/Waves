package com.wavesplatform.lang.v1

import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.Terms

object ContractLimits {
  val MaxComplexityByVersion: StdLibVersion => Int = {
    case V1 | V2 => 2000
    case _       => 4000
  }

  val MaxCallableComplexityByVersion: StdLibVersion => Int = {
    case V1 | V2 | V3 | V4 => 4000
    case V5                => 10000
    case _                 => 52000
  }

  val MaxTotalInvokeComplexity: StdLibVersion => Int = {
    case v @ (V1 | V2 | V3 | V4) => MaxComplexityByVersion(v) * (MaxAttachedPaymentAmount + MaxCallableActionsAmountBeforeV6(V4) + 1)
    case V5                      => 26000
    case _                       => 52000
  }

  val MaxSyncDAppCalls: StdLibVersion => Int =
    _ => 100

  // used after activation of BlockV5
  val MaxAccountVerifierComplexityByVersion: StdLibVersion => Int =
    _ => 2000

  val FailFreeInvokeComplexity = 1000
  val FreeVerifierComplexity   = 200

  val MaxExprSizeInBytes: Int     = 8 * 1024
  val MaxContractSizeInBytes: Int = 32 * 1024
  val MaxContractSizeInBytesV6    = 160 * 1024

  val MaxContractMetaSizeInBytes = 1024

  // As in Scala
  val MaxInvokeScriptArgs       = 22
  val MaxDeclarationNameInBytes = 255

  // Data 0.001 per kilobyte, rounded up, fee for CI is 0.005
  val MaxInvokeScriptSizeInBytes: Int = 5 * 1024
  val MaxWriteSetSizeInBytes: Int     = 5 * 1024
  val MaxWriteSetSize: Int            = 100

  val MaxTotalWriteSetSizeInBytes: Int = 15 * 1024

  // should conform DataEntry limits
  val MaxKeySizeInBytesByVersion: StdLibVersion => Int =
    v => if (v >= V4) 400 else 100

  // Mass Transfer	0.001 + 0.0005*N, rounded up to 0.001, fee for CI is 0.005
  def MaxCallableActionsAmountBeforeV6(v: StdLibVersion): Int =
    v match {
      case version if version < V5 => 10
      case _                       => 30
    }

  val MaxBalanceScriptActionsAmountV6: Int = 100
  val MaxAssetScriptActionsAmountV6: Int   = 30

  val MaxAttachedPaymentAmount    = 2
  val MaxAttachedPaymentAmountV5  = 10
  val MaxTotalPaymentAmountRideV6 = 100

  // Data weight related constants
  val OBJ_WEIGHT      = 40L
  val FIELD_WEIGHT    = 30L
  val EMPTYARR_WEIGHT = 20L
  val ELEM_WEIGHT     = 20L
  val MaxWeight: Long =
    Terms.DataTxMaxProtoBytes * 2L +                                          // bodyBytes and data
      32L + 8L + 8L + 8L +                                                    // header
      OBJ_WEIGHT + FIELD_WEIGHT + 32L +                                       // address object
      EMPTYARR_WEIGHT + (ELEM_WEIGHT + 64L) * 8L +                            // proofs
      EMPTYARR_WEIGHT + (ELEM_WEIGHT + OBJ_WEIGHT + FIELD_WEIGHT * 2L) * 100L // Data entries

  val MaxCmpWeight = 13000

  val MinTupleSize = 2
  val MaxTupleSize = 22
}
