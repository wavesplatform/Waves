package com.wavesplatform.transaction.utils

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.utils.EthEncoding
import org.web3j.crypto.{Bip32ECKeyPair, ECKeyPair, Keys}

object EthConverters {
  implicit class EthereumKeyPairExt(private val kp: KeyPair) extends AnyVal {
    def toEthKeyPair: Bip32ECKeyPair = Bip32ECKeyPair.generateKeyPair(kp.seed)
    def toEthAddress: String         = toEthKeyPair.toEthAddress
    def toEthWavesAddress: Address   = toEthKeyPair.toWavesAddress
  }

  implicit class EthereumAddressExt(private val address: Address) extends AnyVal {
    def toEthAddress: String = EthEncoding.toHexString(address.publicKeyHash)
  }

  implicit class EthereumECKeyPairExt(private val kp: ECKeyPair) extends AnyVal {
    def toWavesAddress: Address   = Address(EthEncoding.toBytes(toEthAddress))
    def toEthAddress: String = Keys.getAddress(kp)
  }
}
