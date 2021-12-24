package com.wavesplatform.utils

import java.math.BigInteger

import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.{EthereumTransaction, TxHelpers}
import com.wavesplatform.transaction.assets.exchange.OrderSender
import com.wavesplatform.transaction.utils.EthTxGenerator
import org.scalatest.{BeforeAndAfterEach, Suite}
import org.web3j.crypto.{Bip32ECKeyPair, RawTransaction, SignedRawTransaction}
import org.web3j.crypto.Sign.SignatureData

trait EthHelpers {
  val EthStubBytes32: Array[Byte] = Array.fill(32)(EthChainId.byte)

  object EthSignature {
    def apply(str: String): OrderSender.Eip712Signature = OrderSender.Eip712Signature(ByteStr(EthEncoding.toBytes(str)))
  }

  val TestEthOrdersPublicKey: PublicKey = PublicKey(
    EthEncoding.toBytes(TxHelpers.defaultEthSigner.getPublicKey.toString(16))
  )

  val TestEthRawTransaction: RawTransaction =
    RawTransaction.createTransaction(
      BigInteger.valueOf(System.currentTimeMillis()),
      EthereumTransaction.GasPrice,
      EthereumTransaction.GasPrice,
      "",
      (BigInt(123) * EthereumTransaction.AmountMultiplier).bigInteger,
      ""
    )

  val TestEthSignature: SignatureData =
    EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, 'E'.toByte)(TestEthRawTransaction).signatureData

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

  implicit class EthTransactionTestExt(tx: EthereumTransaction) {
    def toSignedRawTransaction: SignedRawTransaction = new SignedRawTransaction(tx.underlying.getTransaction, tx.signatureData)
  }
}

trait EthSetChainId extends BeforeAndAfterEach with EthHelpers { self: Suite =>
  abstract override protected def beforeEach(): Unit = {
    super.beforeEach()
    EthChainId.set()
  }
  abstract override protected def afterEach(): Unit = {
    EthChainId.unset()
    super.afterEach()
  }
}
