package com.wavesplatform.lang.v1.evaluator

object FunctionIds {
  val SUM_LONG: Short = 0
  val SUB_LONG: Short = 1
  val GT_LONG: Short  = 4
  val GE_LONG: Short  = 5 // ?

  val SUM_STRING: Short = 50
  val GT_STRING: Short  = 51
  val GE_STRING: Short  = 52 // ?

  val SUM_BYTES: Short = 100

  val EQ: Short = 150

  val MINUS_LONG: Short = 200
  val MUL_LONG: Short   = 201
  val DIV_LONG: Short   = 202
  val MOD_LONG: Short   = 203

  val NOT_BOOLEAN: Short = 250

  val SIZE_LIST: Short = 300
  val GET_LIST: Short  = 301

  val SIZE_BYTES: Short = 350
  val TAKE_BYTES: Short = 351

  val SOME: Short      = 400
  val ISDEFINED: Short = 401
  val EXTRACT: Short   = 402

  val ISINSTANCEOF: Short = 450

  val FRACTION: Short = 500

  // Crypto
  val SIGVERIFY: Short = 550
  val KECCAK256: Short = 551
  val BLAKE256: Short  = 552
  val SHA256: Short    = 553

  val TOBASE58: Short = 600
  val TOBASE64: Short = 601

  // Waves
  val DATA_LONG: Short    = 650
  val DATA_BOOLEAN: Short = 651
  val DATA_BYTES: Short   = 652
  val DATA_STRING: Short  = 653

  val ADDRESSFROMPUBKEY: Short    = 700
  val ADDRESSFROMSTRING: Short    = 701
  val ADDRESSFROMRECIPIENT: Short = 702

  val GETTRANSACTIONBYID: Short    = 750
  val ACCOUNTBALANCE: Short        = 751
  val ACCOUNTASSETBALANCE: Short   = 752
  val TRANSACTIONHEIGHTBYID: Short = 753
}
