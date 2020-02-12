package com.wavesplatform.it

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto

import scala.util.Random

case class Account(
    nodes: Seq[Node],
    private[this] val seedPhrase: Option[String] = None
) {

  val seed: String  = seedPhrase.getOrElse(Random.nextString(10))
  val keys: KeyPair = KeyPair(ByteStr.fromByteArray(seed.getBytes))

  val privateKey: String = keys.privateKey.toString
  val publicKey: String  = keys.publicKey.toString
  val address: String    = keys.toAddress.stringRepr

  def sign(bytes: ByteStr): ByteStr = crypto.sign(keys.privateKey, bytes)

}
