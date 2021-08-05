package com.wavesplatform.utils

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import org.scalatest.{BeforeAndAfterAll, Suite}

trait EthHelpers {
  val EthTestChainId: Byte        = 'E'.toByte
  val EthStubBytes32: Array[Byte] = Array.fill(32)(EthTestChainId)

  object EthPublicKey {
    def apply(str: String): PublicKey = PublicKey(EthEncoding.toBytes(str))
  }

  object EthSignature {
    def apply(str: String): Option[ByteStr] = Some(ByteStr(EthEncoding.toBytes(str)))
  }

  val TestEthPublicKey: PublicKey = EthPublicKey(
    "0xd10a150ba9a535125481e017a09c2ac6a1ab43fc43f7ab8f0d44635106672dd7de4f775c06b730483862cbc4371a646d86df77b3815593a846b7272ace008c42"
  )
}

trait EthSetChainId extends BeforeAndAfterAll with EthHelpers { self: Suite =>
  abstract override protected def beforeAll(): Unit = {
    AddressScheme.current = new AddressScheme {
      val chainId: Byte = EthTestChainId
    }
    super.beforeAll()
  }
  abstract override protected def afterAll(): Unit = {
    AddressScheme.current = new AddressScheme {
      val chainId: Byte = 'T'.toByte
    }
    super.afterAll()
  }
}
