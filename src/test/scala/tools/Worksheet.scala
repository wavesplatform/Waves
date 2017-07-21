package tools

import com.wavesplatform.state2.ByteStr
import scorex.crypto.hash.SecureCryptographicHash

object Worksheet extends App {

  val key = "_"
  println(ByteStr(SecureCryptographicHash.hash(key)))
}
