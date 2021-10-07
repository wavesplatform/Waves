package com.wavesplatform.utils

import java.math.BigInteger

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.TestTime
import com.wavesplatform.transaction.TransactionType
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.web3j.crypto.RawTransaction
import org.web3j.crypto.Sign.SignatureData
import org.web3j.utils.Numeric

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

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  val TestEthFee = FeeUnit * FeeConstants(TransactionType.Transfer)

  val TestEthUnderlying = RawTransaction.createTransaction(BigInteger.valueOf(ts), BigInteger.ZERO, BigInteger.valueOf(TestEthFee), "", "")

  val TestEthSignature = new SignatureData(
    28.toByte,
    Numeric.hexStringToByteArray("0x0464eee9e2fe1a10ffe48c78b80de1ed8dcf996f3f60955cb2e03cb21903d930"),
    Numeric.hexStringToByteArray("0x06624da478b3f862582e85b31c6a21c6cae2eee2bd50f55c93c4faad9d9c8d7f")
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
