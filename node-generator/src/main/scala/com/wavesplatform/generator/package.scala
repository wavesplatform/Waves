package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.utils.EthEncoding
import org.web3j.crypto.{Bip32ECKeyPair, Keys}

package object generator {
  implicit class EthereumKeyPairExt(kp: KeyPair) {
    def toEthKeyPair: Bip32ECKeyPair = Bip32ECKeyPair.generateKeyPair(kp.seed)
    def toEthAddress: String = Keys.getAddress(toEthKeyPair)
    def toEthWavesAddress: Address = Address(EthEncoding.toBytes(toEthAddress))
  }

  implicit class EthereumAddressExt(address: Address) {
    def toEthAddress: String = EthEncoding.toHexString(address.publicKeyHash)
  }
}
