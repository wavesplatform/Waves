package com.wavesplatform.lang.v1.evaluator

object FunctionIds {

  val EQ: Short           = 0
  val ISINSTANCEOF: Short = 1
  val THROW: Short        = 2

  val SUM_LONG: Short = 100
  val SUB_LONG: Short = 101
  val GT_LONG: Short  = 102
  val GE_LONG: Short  = 103
  val MUL_LONG: Short = 104
  val DIV_LONG: Short = 105
  val MOD_LONG: Short = 106
  val FRACTION: Short = 107

  val POW: Short = 108
  val LOG: Short = 109

  val SIZE_BYTES: Short = 200
  val TAKE_BYTES: Short = 201
  val DROP_BYTES: Short = 202
  val SUM_BYTES: Short  = 203

  val SUM_STRING: Short  = 300
  val TAKE_STRING: Short = 303
  val DROP_STRING: Short = 304
  val SIZE_STRING: Short = 305

  val SIZE_LIST: Short         = 400
  val GET_LIST: Short          = 401
  val LONG_TO_BYTES: Short     = 410
  val STRING_TO_BYTES: Short   = 411
  val BOOLEAN_TO_BYTES: Short  = 412
  val LONG_TO_STRING: Short    = 420
  val BOOLEAN_TO_STRING: Short = 421

  val CREATE_LIST: Short = 1100

  val UTF8STRING: Short   = 1200
  val BININT: Short       = 1201
  val BININT_OFF: Short   = 1202
  val INDEXOF: Short      = 1203
  val INDEXOFN: Short     = 1204
  val SPLIT: Short        = 1205
  val PARSEINT: Short     = 1206
  val LASTINDEXOF: Short  = 1207
  val LASTINDEXOFN: Short = 1208

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

  // Waves
  val GETTRANSACTIONBYID: Short      = 1000
  val TRANSACTIONHEIGHTBYID: Short   = 1001
  val ACCOUNTASSETBALANCE: Short     = 1003
  val GETASSETINFOBYID: Short        = 1004
  val BLOCKINFOBYHEIGHT: Short       = 1005
  val TRANSFERTRANSACTIONBYID: Short = 1006

  val DATA_LONG_FROM_ARRAY: Short    = 1040
  val DATA_BOOLEAN_FROM_ARRAY: Short = 1041
  val DATA_BYTES_FROM_ARRAY: Short   = 1042
  val DATA_STRING_FROM_ARRAY: Short  = 1043

  val DATA_LONG_FROM_STATE: Short    = 1050
  val DATA_BOOLEAN_FROM_STATE: Short = 1051
  val DATA_BYTES_FROM_STATE: Short   = 1052
  val DATA_STRING_FROM_STATE: Short  = 1053

  val ADDRESSFROMRECIPIENT: Short = 1060
  val ADDRESSTOSTRING: Short      = 1061

}
