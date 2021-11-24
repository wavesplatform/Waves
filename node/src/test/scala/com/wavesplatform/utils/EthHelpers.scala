package com.wavesplatform.utils

import java.math.BigInteger

import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.TestTime
import com.wavesplatform.transaction.{EthereumTransaction, TxHelpers}
import com.wavesplatform.transaction.utils.EthTxGenerator
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.web3j.crypto.{Bip32ECKeyPair, RawTransaction}
import org.web3j.crypto.Sign.SignatureData

trait EthHelpers {
  val EthStubBytes32: Array[Byte] = Array.fill(32)(EthChainId.byte)

  object EthSignature {
    def apply(str: String): Option[ByteStr] = Some(ByteStr(EthEncoding.toBytes(str)))
  }

  val TestEthPublicKey: PublicKey = PublicKey(EthEncoding.toBytes(EthEncoding.toHexString(TxHelpers.defaultEthSigner.getPublicKey: BigInt)))

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  val TestEthUnderlying: RawTransaction =
    RawTransaction.createTransaction(
      BigInteger.valueOf(ts),
      EthereumTransaction.GasPrice,
      EthereumTransaction.GasPrice,
      "",
      (BigInt(123) * EthereumTransaction.AmountMultiplier).bigInteger,
      ""
    )

  val TestEthSignature: SignatureData =
    EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, 'T'.toByte)(TestEthUnderlying).signatureData

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
    def defaultEthAddress: Address       = helpers.defaultSigner.toEthWavesAddress
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
