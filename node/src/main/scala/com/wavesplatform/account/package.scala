package com.wavesplatform

package object account {
  type PublicKey  = PublicKey.Type
  type PrivateKey = PrivateKey.Type

  type AddressOrAlias = Either[WavesAddress, Alias]

  implicit class AddressOrAliasExt(val aoa: AddressOrAlias) extends AnyVal {
    def bytes: Array[Byte] = aoa.fold(_.bytes, _.bytes)
    def chainId: Byte = aoa.fold(_.chainId, _.chainId)
    def recipient: Recipient = aoa.fold(identity, identity)
  }
}
