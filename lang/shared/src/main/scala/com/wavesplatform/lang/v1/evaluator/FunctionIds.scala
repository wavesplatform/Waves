package com.wavesplatform.lang.v1.evaluator

object FunctionIds {
  val SUM_LONG: Short = 0
  val SUB_LONG: Short = 1
  val GT_LONG: Short  = 4
  val GE_LONG: Short  = 5

  val SUM_STRING: Short = 6
  val GT_STRING: Short  = 9
  val GE_STRING: Short  = 10

  val SUM_BYTES: Short = 11

  val EQ: Short = 14

  val MUL_LONG: Short = 16
  val DIV_LONG: Short = 17
  val MOD_LONG: Short = 18

  val MINUS_LONG: Short  = 19
  val NOT_BOOLEAN: Short = 20

  val GET_LIST: Short  = 21
  val SIZE_LIST: Short = 22

  val SIZE_BYTES: Short = 23

  val SOME: Short      = 24
  val ISDEFINED: Short = 25
  val EXTRACT: Short   = 26

  val ISINSTANCEOF: Short = 27

  val FRACTION: Short = 28

  // Crypto
  val SIGVERIFY: Short = 64

  val KECCAK256: Short = 65
  val BLAKE256: Short  = 66
  val SHA256: Short    = 67

  val TOBASE58: Short = 68
  val TOBASE64: Short = 69

  // Waves
  val DATA_LONG: Short    = 128
  val DATA_BOOLEAN: Short = 129
  val DATA_BYTES: Short   = 130
  val DATA_STRING: Short  = 131

  val ADDRESSFROMPUBKEY: Short    = 132
  val ADDRESSFROMSTRING: Short    = 133
  val ADDRESSFROMRECIPIENT: Short = 134

  val GETTRANSACTIONBYID: Short    = 135
  val ACCOUNTBALANCE: Short        = 136
  val ACCOUNTASSETBALANCE: Short   = 137
  val TRANSACTIONHEIGHTBYID: Short = 138
}
