package com.wavesplatform.utils

import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.web3j.crypto.Bip32ECKeyPair

trait EthHelpers {
  val EthStubBytes32: Array[Byte] = Array.fill(32)(EthChainId.byte)

  object EthPublicKey {
    def apply(str: String): PublicKey = PublicKey(EthEncoding.toBytes(str))
  }

  object EthSignature {
    def apply(str: String): Option[ByteStr] = Some(ByteStr(EthEncoding.toBytes(str)))
  }

  val TestEthPublicKey: PublicKey = EthPublicKey(
    "0xd10a150ba9a535125481e017a09c2ac6a1ab43fc43f7ab8f0d44635106672dd7de4f775c06b730483862cbc4371a646d86df77b3815593a846b7272ace008c42"
  )

  object EthChainId {
    val byte: Byte = 'E'.toByte

    def set(): Unit = {
      AddressScheme.current = new AddressScheme {
        val chainId: Byte = EthChainId.byte
      }
    }

    def unset(): Unit = {
      AddressScheme.current = new AddressScheme {
        val chainId: Byte = 'T'.toByte
      }
    }

    def withEChainId[T](f: => T): T = {
      this.set()
      try f
      finally this.unset()
    }
  }

  implicit class TxHelpersEthExt(helpers: TxHelpers.type) {
    import com.wavesplatform.transaction.utils.EthConverters._
    def defaultEthSigner: Bip32ECKeyPair = helpers.defaultSigner.toEthKeyPair
    def defaultEthAddress: Address = helpers.defaultSigner.toEthWavesAddress
  }
}

trait EthSetChainId extends BeforeAndAfterAll with EthHelpers { self: Suite =>
  abstract override protected def beforeAll(): Unit = {
    EthChainId.set()
    super.beforeAll()
  }
  abstract override protected def afterAll(): Unit = {
    EthChainId.unset()
    super.afterAll()
  }
}
