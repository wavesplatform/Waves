package com.wavesplatform.transaction.utils

import com.wavesplatform.account.{Address, KeyPair, PKKeyPair, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.utils.EthEncoding
import org.web3j.crypto.{Bip32ECKeyPair, ECKeyPair, Keys}

object EthConverters {
  implicit class EthereumKeyPairExt(private val kp: KeyPair) extends AnyVal {
    def toEthKeyPair: Bip32ECKeyPair = kp match {
      case skp: SeedKeyPair => Bip32ECKeyPair.generateKeyPair(skp.seed)
      case pkkp: PKKeyPair  => Bip32ECKeyPair.create(pkkp.privateKey.arr, Array.emptyByteArray)
    }
    def toEthAddress: String       = toEthKeyPair.toEthAddress
    def toEthWavesAddress: Address = toEthKeyPair.toWavesAddress
  }

  implicit class EthereumAddressExt(private val address: Address) extends AnyVal {
    def toEthAddress: String = EthEncoding.toHexString(address.publicKeyHash)
  }

  implicit class EthereumECKeyPairExt(private val kp: ECKeyPair) extends AnyVal {
    def toWavesAddress: Address = Address(EthEncoding.toBytes(toEthAddress))
    def toEthAddress: String    = Keys.getAddress(kp)
  }

  implicit class EthereumByteStrExt(private val bs: ByteStr) extends AnyVal {
    def toHexString: String = EthEncoding.toHexString(bs.arr)
  }
}
