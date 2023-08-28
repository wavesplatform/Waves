package com.wavesplatform.lang.v1.evaluator

object FunctionIds {

  val EQ: Short           = 0
  val ISINSTANCEOF: Short = 1
  val THROW: Short        = 2
  val GET_TYPE: Short     = 3

  val SUM_LONG: Short = 100
  val SUB_LONG: Short = 101
  val GT_LONG: Short  = 102
  val GE_LONG: Short  = 103
  val MUL_LONG: Short = 104
  val DIV_LONG: Short = 105
  val MOD_LONG: Short = 106

  val FRACTION: Short        = 107
  val FRACTION_ROUNDS: Short = 110

  val POW: Short = 108
  val LOG: Short = 109

  val POW_BIGINT: Short = 118
  val LOG_BIGINT: Short = 119

  val SIZE_BYTES: Short       = 200
  val TAKE_BYTES: Short       = 201
  val DROP_BYTES: Short       = 202
  val SUM_BYTES: Short        = 203
  val TAKE_RIGHT_BYTES: Short = 204
  val DROP_RIGHT_BYTES: Short = 205

  val SUM_STRING: Short        = 300
  val TAKE_STRING: Short       = 303
  val DROP_STRING: Short       = 304
  val SIZE_STRING: Short       = 305
  val TAKE_RIGHT_STRING: Short = 306
  val DROP_RIGHT_STRING: Short = 307

  val TO_BIGINT: Short              = 310
  val SUM_BIGINT: Short             = 311
  val SUB_BIGINT: Short             = 312
  val MUL_BIGINT: Short             = 313
  val DIV_BIGINT: Short             = 314
  val MOD_BIGINT: Short             = 315
  val FRACTION_BIGINT: Short        = 316
  val FRACTION_BIGINT_ROUNDS: Short = 317
  val UMINUS_BIGINT: Short          = 318
  val GT_BIGINT: Short              = 319
  val GE_BIGINT: Short              = 320

  val SIZE_LIST: Short           = 400
  val GET_LIST: Short            = 401
  val MEDIAN_LIST: Short         = 405
  val MAX_LIST: Short            = 406
  val MIN_LIST: Short            = 407
  val MAX_LIST_BIGINT: Short     = 408
  val MIN_LIST_BIGINT: Short     = 409
  val LONG_TO_BYTES: Short       = 410
  val STRING_TO_BYTES: Short     = 411
  val BOOLEAN_TO_BYTES: Short    = 412
  val BIGINT_TO_BYTES: Short     = 413
  val BYTES_TO_BIGINT: Short     = 414
  val BYTES_TO_BIGINT_LIM: Short = 415
  val BIGINT_TO_INT: Short       = 416
  val LONG_TO_STRING: Short      = 420
  val BOOLEAN_TO_STRING: Short   = 421
  val BIGINT_TO_STRING: Short    = 422
  val STRING_TO_BIGINT: Short    = 423
  val STRING_TO_BIGINTOPT: Short = 424
  val MEDIAN_LISTBIGINT: Short   = 425

  val CREATE_LIST: Short             = 1100
  val APPEND_LIST: Short             = 1101
  val CONCAT_LIST: Short             = 1102
  val INDEX_OF_LIST: Short           = 1103
  val LAST_INDEX_OF_LIST: Short      = 1104
  val REMOVE_BY_INDEX_OF_LIST: Short = 1105

  val UTF8STRING: Short   = 1200
  val BININT: Short       = 1201
  val BININT_OFF: Short   = 1202
  val INDEXOF: Short      = 1203
  val INDEXOFN: Short     = 1204
  val SPLIT: Short        = 1205
  val PARSEINT: Short     = 1206
  val LASTINDEXOF: Short  = 1207
  val LASTINDEXOFN: Short = 1208
  val MAKESTRING: Short   = 1209

  val MAKESTRING2C: Short  = 1210
  val MAKESTRING11C: Short = 1211
  val SPLIT4C: Short       = 1212
  val SPLIT51C: Short      = 1213

  val CREATE_TUPLE: Short = 1300 // Reserved 22 id for tuple constructors
  val SIZE_TUPLE: Short   = 1350

  // Crypto
  val SIGVERIFY: Short = 500
  val KECCAK256: Short = 501
  val BLAKE256: Short  = 502
  val SHA256: Short    = 503
  val RSAVERIFY: Short = 504

  val TOBASE58: Short   = 600
  val FROMBASE58: Short = 601
  val TOBASE64: Short   = 602
  val FROMBASE64: Short = 603
  val TOBASE16: Short   = 604
  val FROMBASE16: Short = 605

  val CHECK_MERKLE_PROOF: Short = 700
  val CREATE_MERKLE_ROOT: Short = 701

  val BLS12_GROTH16_VERIFY: Short = 800
  val BN256_GROTH16_VERIFY: Short = 801

  val ECRECOVER: Short = 900

  val BLS12_GROTH16_VERIFY_LIM: Short = 2400 // Reserved n id for generated limited functions
  val BN256_GROTH16_VERIFY_LIM: Short = 2450 // Reserved n id for generated limited functions
  val SIGVERIFY_LIM: Short            = 2500 // Reserved n id for generated limited functions
  val RSAVERIFY_LIM: Short            = 2600 // Reserved n id for generated limited functions
  val KECCAK256_LIM: Short            = 2700 // Reserved n id for generated limited functions
  val BLAKE256_LIM: Short             = 2800 // Reserved n id for generated limited functions
  val SHA256_LIM: Short               = 2900 // Reserved n id for generated limited functions

  // Waves
  val GETTRANSACTIONBYID: Short      = 1000
  val TRANSACTIONHEIGHTBYID: Short   = 1001
  val ACCOUNTASSETBALANCE: Short     = 1003
  val GETASSETINFOBYID: Short        = 1004
  val BLOCKINFOBYHEIGHT: Short       = 1005
  val TRANSFERTRANSACTIONBYID: Short = 1006
  val ACCOUNTWAVESBALANCE: Short     = 1007
  val ACCOUNTASSETONLYBALANCE: Short = 1008
  val ACCOUNTSCRIPTHASH: Short       = 1009

  val CALLDAPP: Short          = 1020
  val CALLDAPPREENTRANT: Short = 1021

  val DATA_LONG_FROM_ARRAY: Short    = 1040
  val DATA_BOOLEAN_FROM_ARRAY: Short = 1041
  val DATA_BYTES_FROM_ARRAY: Short   = 1042
  val DATA_STRING_FROM_ARRAY: Short  = 1043

  val DATA_LONG_FROM_STATE: Short    = 1050
  val DATA_BOOLEAN_FROM_STATE: Short = 1051
  val DATA_BYTES_FROM_STATE: Short   = 1052
  val DATA_STRING_FROM_STATE: Short  = 1053

  val IS_UNTOUCHED: Short = 1054

  val DATA_LONG_FROM_STATE_SELF: Short    = 1055
  val DATA_BOOLEAN_FROM_STATE_SELF: Short = 1056
  val DATA_BYTES_FROM_STATE_SELF: Short   = 1057
  val DATA_STRING_FROM_STATE_SELF: Short  = 1058

  val ADDRESSFROMRECIPIENT: Short        = 1060
  val ADDRESSTOSTRING: Short             = 1061
  val ADDRESSFROMSTRING_NATIVE: Short    = 1062
  val ADDRESSFROMPUBLICKEY_NATIVE: Short = 1063

  val TRANSFER_TRANSACTION_FROM_PROTO: Short = 1070

  val CALCULATE_ASSET_ID: Short = 1080
  val CALCULATE_LEASE_ID: Short = 1081

  val SIMPLIFIED_ISSUE_ACTION_CONSTRUCTOR: Short = 1090
  val DETAILED_ISSUE_ACTION_CONSTRUCTOR: Short   = 1091
  val SIMPLIFIED_LEASE_ACTION_CONSTRUCTOR: Short = 1092
  val DETAILED_LEASE_ACTION_CONSTRUCTOR: Short   = 1093
}
