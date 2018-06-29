package com.wavesplatform.lang.v1.evaluator

object FunctionIds {

  val EQ: Short           = 0
  val ISINSTANCEOF: Short = 1

  val SUM_LONG: Short = 100
  val SUB_LONG: Short = 101
  val GT_LONG: Short  = 102
  val GE_LONG: Short  = 103
  val MUL_LONG: Short = 104
  val DIV_LONG: Short = 105
  val MOD_LONG: Short = 106
  val FRACTION: Short = 107

  val SIZE_BYTES: Short = 200
  val TAKE_BYTES: Short = 201
  val DROP_BYTES: Short = 202
  val SUM_BYTES: Short  = 203

  val SUM_STRING: Short  = 300
  val TAKE_STRING: Short = 303
  val DROP_STRING: Short = 304
  val SIZE_STRING: Short = 305

  val SIZE_LIST: Short = 400
  val GET_LIST: Short  = 401

  // Crypto
  val SIGVERIFY: Short = 500
  val KECCAK256: Short = 501
  val BLAKE256: Short  = 502
  val SHA256: Short    = 503

  val TOBASE58: Short   = 600
  val FROMBASE58: Short = 601
  val TOBASE64: Short   = 602
  val FROMBASE64: Short = 603

  // Waves
  val GETTRANSACTIONBYID: Short    = 700
  val TRANSACTIONHEIGHTBYID: Short = 701
  val ACCOUNTBALANCE: Short        = 702
  val ACCOUNTASSETBALANCE: Short   = 703
  val DATA_LONG: Short             = 750
  val DATA_BOOLEAN: Short          = 751
  val DATA_BYTES: Short            = 752
  val DATA_STRING: Short           = 753
  val ADDRESSFROMRECIPIENT: Short  = 760
  val ADDRESSFROMBYTES: Short      = 761

}
