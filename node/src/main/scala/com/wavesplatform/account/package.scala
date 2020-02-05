package com.wavesplatform

package object account {
  type PublicKey  = PublicKey.Type
  type PrivateKey = PrivateKey.Type

  type ChainId = Byte
  object ChainId {
    def current: ChainId = AddressScheme.current.chainId
  }
}
