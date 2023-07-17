package com.wavesplatform.transaction.smart

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.test.{FlatSpec, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers}
import com.wavesplatform.utils.{EthEncoding, EthHelpers}
import com.wavesplatform.TestValues
import org.scalatest.{BeforeAndAfterAll, Inside}
import org.web3j.crypto.{Bip32ECKeyPair, RawTransaction, Sign, SignedRawTransaction, TransactionEncoder}

class EthereumTransactionSpec extends FlatSpec with BeforeAndAfterAll with WithDomain with EthHelpers with Inside {

  val TestAsset: IssuedAsset = TestValues.asset

  "Ethereum transfer" should "recover correct key (NODE-646)" in {
    val senderAccount = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress = TxHelpers.defaultSigner.toEthWavesAddress
    val transaction   = EthTxGenerator.generateEthTransfer(senderAccount, senderAddress, 1, Waves)
    transaction.senderAddress() shouldBe senderAccount.toWavesAddress
  }

  it should "recover correct key with leading zeros (NODE-874)" in {
    val senderAcc = Bip32ECKeyPair.create(
      EthEncoding.toBytes("0x00db4a036ea48572bf27630c72a1513f48f0b4a6316606fd01c23318befdf984"),
      Array.emptyByteArray
    )
    val tx = EthTxGenerator.generateEthTransfer(senderAcc, senderAcc.toWavesAddress, 1, Waves)
    EthEncoding.toHexString(
      tx.signerPublicKey().arr
    ) shouldBe "0x00d7cf9ff594b07273228e7dd591707d38a1dba0a39492fd64445ba9cbb3bf66c862b9752f02bf8d1a0f00ccb11ae550a7616bd965c10f0101202d75580786ee"
  }

  it should "recover correct address chainId (NODE-647)" in {
    val transfer      = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, Waves)
    val assetTransfer = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, TestValues.asset)
    val invoke        = EthTxGenerator.generateEthInvoke(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, "test", Nil, Nil)

    inside(EthereumTransaction(transfer.toSignedRawTransaction).explicitGet().payload) { case t: EthereumTransaction.Transfer =>
      t.recipient.chainId shouldBe 'T'.toByte
    }

    inside(EthereumTransaction(assetTransfer.toSignedRawTransaction).explicitGet().payload) { case t: EthereumTransaction.Transfer =>
      t.recipient.chainId shouldBe 'T'.toByte
    }

    inside(EthereumTransaction(invoke.toSignedRawTransaction).explicitGet().payload) { case t: EthereumTransaction.Invocation =>
      t.dApp.chainId shouldBe 'T'.toByte
    }
  }

  it should "change id if signature is changed (NODE-648)" in {
    val senderAccount = TxHelpers.defaultSigner.toEthKeyPair
    val secondAccount = TxHelpers.secondSigner.toEthKeyPair
    val transaction1  = EthTxGenerator.generateEthTransfer(senderAccount, TxHelpers.defaultAddress, 1, Waves)
    val transaction2  = EthTxGenerator.signRawTransaction(secondAccount, AddressScheme.current.chainId)(transaction1.underlying)
    transaction1.id() shouldNot be(transaction2.id())
  }

  it should "reject legacy transactions (NODE-649)" in {
    val senderAccount     = TxHelpers.defaultEthSigner
    val eip155Transaction = EthTxGenerator.generateEthTransfer(senderAccount, TxHelpers.defaultAddress, 1, Waves)

    val legacyTransaction =
      new SignedRawTransaction(
        eip155Transaction.underlying.getTransaction,
        Sign.signMessage(TransactionEncoder.encode(eip155Transaction.underlying, 1.toLong), senderAccount, true)
      )
    EthereumTransaction(legacyTransaction) should produce("Legacy transactions are not supported")
  }

  it should "fail with empty to field (NODE-875)" in {
    val rawTransaction = RawTransaction.createTransaction(
      BigInt(System.currentTimeMillis()).bigInteger,
      EthereumTransaction.GasPrice,
      BigInt(100000).bigInteger, // fee
      "",                        // empty "to"
      (BigInt(1) * EthereumTransaction.AmountMultiplier).bigInteger,
      ""
    )
    a[RuntimeException] should be thrownBy EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, TxHelpers.defaultAddress.chainId)(
      rawTransaction
    )
  }

  it should "fail with invalid to field (NODE-876)" in {
    val rawTransaction = RawTransaction.createTransaction(
      BigInt(System.currentTimeMillis()).bigInteger,
      EthereumTransaction.GasPrice,
      BigInt(100000).bigInteger, // fee
      "0xffffffff",              // invalid "to"
      (BigInt(1) * EthereumTransaction.AmountMultiplier).bigInteger,
      ""
    )
    a[RuntimeException] should be thrownBy EthTxGenerator.signRawTransaction(TxHelpers.defaultEthSigner, TxHelpers.defaultAddress.chainId)(
      rawTransaction
    )
  }

  it should "not accept zero transfers (NODE-654)" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress
    intercept[RuntimeException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, 0, Waves)).toString should include(
      "Transaction cancellation is not supported"
    )
    intercept[RuntimeException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, 0, TestAsset)).toString should include(
      "NonPositiveAmount"
    )
    intercept[RuntimeException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, -1, Waves)).toString should include(
      "NegativeAmount"
    )
    intercept[UnsupportedOperationException](EthTxGenerator.generateEthTransfer(senderAccount, recipientAddress, -1, TestAsset))
  }

  it should "not accept value + data (NODE-655)" in {
    val senderAccount    = TxHelpers.defaultSigner.toEthKeyPair
    val recipientAddress = TxHelpers.secondSigner.toAddress

    intercept[RuntimeException](
      EthTxGenerator.signRawTransaction(senderAccount, recipientAddress.chainId)(
        RawTransaction.createTransaction(
          BigInt(System.currentTimeMillis()).bigInteger,
          EthereumTransaction.GasPrice,
          BigInt(100000).bigInteger,
          EthEncoding.toHexString(recipientAddress.publicKeyHash),
          (BigInt(100) * EthereumTransaction.AmountMultiplier).bigInteger,
          "0x0000000000"
        )
      )
    ).toString should include(
      "Transaction should have either data or value"
    )
  }

  "Ethereum invoke" should "recover correct key (NODE-659)" in {
    val senderAccount = TxHelpers.defaultSigner.toEthKeyPair
    val senderAddress = TxHelpers.defaultSigner.toEthWavesAddress
    val transaction   = EthTxGenerator.generateEthInvoke(senderAccount, senderAddress, "test", Nil, Nil)
    transaction.senderAddress() shouldBe senderAccount.toWavesAddress
  }

  it should "recover correct key with leading zeros (NODE-877)" in {
    val senderAcc = Bip32ECKeyPair.create(
      EthEncoding.toBytes("0x00db4a036ea48572bf27630c72a1513f48f0b4a6316606fd01c23318befdf984"),
      Array.emptyByteArray
    )
    val tx = EthTxGenerator.generateEthInvoke(senderAcc, senderAcc.toWavesAddress, "test", Nil, Nil)
    EthEncoding.toHexString(
      tx.signerPublicKey().arr
    ) shouldBe "0x00d7cf9ff594b07273228e7dd591707d38a1dba0a39492fd64445ba9cbb3bf66c862b9752f02bf8d1a0f00ccb11ae550a7616bd965c10f0101202d75580786ee"
  }
}
