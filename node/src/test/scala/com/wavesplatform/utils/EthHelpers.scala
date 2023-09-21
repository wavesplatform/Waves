package com.wavesplatform.utils

import java.math.BigInteger
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.EthereumTransaction.AssetDataLength
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers}
import com.wavesplatform.transaction.assets.exchange.OrderAuthentication
import org.web3j.crypto.{Bip32ECKeyPair, RawTransaction, SignedRawTransaction}
import org.web3j.crypto.Sign.SignatureData

trait EthHelpers {
  val EthStubBytes32: Array[Byte] = Array.fill(32)(EthChainId.byte)

  object EthSignature {
    def apply(str: String): OrderAuthentication.Eip712Signature = OrderAuthentication.Eip712Signature(ByteStr(EthEncoding.toBytes(str)))
  }

  val TestEthOrdersPublicKey: PublicKey = PublicKey(
    EthEncoding.toBytes(TxHelpers.defaultEthSigner.getPublicKey.toString(16))
  )

  val TestEthRawTransaction: RawTransaction =
    RawTransaction.createTransaction(
      BigInteger.valueOf(System.currentTimeMillis()),
      EthereumTransaction.GasPrice,
      EthereumTransaction.GasPrice,
      EthEncoding.toHexString(new Array[Byte](20)),
      "0x" + "1" * AssetDataLength
    )

  val TestEthSignature: SignatureData =
    EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, 'T'.toByte)(TestEthRawTransaction).signatureData

  object EthChainId {
    val byte: Byte = 'E'.toByte
  }

  implicit class TxHelpersEthExt(helpers: TxHelpers.type) {
    import com.wavesplatform.transaction.utils.EthConverters.*
    def defaultEthSigner: Bip32ECKeyPair = helpers.defaultSigner.toEthKeyPair
    def defaultEthAddress: Address       = helpers.defaultSigner.toEthWavesAddress
  }

  implicit class EthTransactionTestExt(tx: EthereumTransaction) {
    def toSignedRawTransaction: SignedRawTransaction = new SignedRawTransaction(tx.underlying.getTransaction, tx.signatureData)
  }
}
