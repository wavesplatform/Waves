package com.wavesplatform

package object account {
  type PublicKey  = PublicKey.Type
  type PrivateKey = PrivateKey.Type

  type AddressOrAlias = WavesRecipient
  val AddressOrAlias: WavesRecipient.type = WavesRecipient
}
