package com.wavesplatform.utils

import org.web3j.utils.Numeric

object EthEncoding {
  def toHexString(bs: Array[Byte]): String      = Numeric.toHexString(bs)
  def toHexString(bs: BigInt): String           = Numeric.toHexStringWithPrefix(bs.bigInteger)
  def toBytes(hexString: String): Array[Byte]   = Numeric.hexStringToByteArray(hexString)
  def cleanHexPrefix(hexString: String): String = Numeric.cleanHexPrefix(hexString)
}
