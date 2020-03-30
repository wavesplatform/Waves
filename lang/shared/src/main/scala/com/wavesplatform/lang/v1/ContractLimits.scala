package com.wavesplatform.lang.v1

import com.wavesplatform.lang.directives.values.{StdLibVersion, V1, V2, V3}

object ContractLimits {
  val MaxComplexityByVersion: StdLibVersion => Int = {
    case V1 | V2 => 2000
    case V3      => 4000
  }
  val MaxExprSizeInBytes     =  8 * 1024
  val MaxContractSizeInBytes = 32 * 1024

  val MaxContractMetaSizeInBytes = 1024

  // As in Scala
  val MaxInvokeScriptArgs = 22
  val MaxAnnotatedFunctionNameInBytes    = 255

  // Data	0.001 per kilobyte, rounded up, fee for CI is 0.005
  val MaxInvokeScriptSizeInBytes = 5 * 1024
  val MaxWriteSetSizeInBytes     = 5 * 1024
  val MaxWriteSetSize            = 100
  val MaxKeySizeInBytes          = 100

  // Mass Transfer	0.001 + 0.0005*N, rounded up to 0.001, fee for CI is 0.005
  val MaxPaymentAmount = 10

  // Data weight related constants
  val OBJ_WEIGHT = 40l
  val FIELD_WEIGHT = 30l
  val EMPTYARR_WEIGHT = 20l
  val ELEM_WEIGHT = 20l
  val MaxWeight =
    150l * 1024l * 2l // MaxBytes dublicate in bodyBytes and data
  + 32l + 8l + 8l + 8l // header
  + OBJ_WEIGHT + FIELD_WEIGHT + 32l // address object
  + EMPTYARR_WEIGHT + (ELEM_WEIGHT + 64l) * 8l // proofs
  + EMPTYARR_WEIGHT + (ELEM_WEIGHT + OBJ_WEIGHT + FIELD_WEIGHT * 2l) * 100l // Data entries

  val MaxCmpWeight = 13000l
}
