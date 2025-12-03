package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V8
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.EthTxGenerator.{generateEthInvoke, generateEthTransfer}
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.EthereumKeyPairExt

import scala.util.Try

class TransactionAssetChecksTest extends PropSpec with WithDomain {
  private val dApp = TestCompiler(V8).compileContract(
    """
      | @Callable(i)
      | func default() = []
    """.stripMargin
  )
  private val issueTx = issue(secondSigner)
  private val asset   = IssuedAsset(issueTx.id())

  property("invoke script transaction") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(secondSigner, dApp), issueTx)
      d.appendBlockE(invoke(secondAddress, payments = Seq(Payment(1, IssuedAsset(ByteStr.fill(31)(1)))))) should produce(
        "invalid asset ID 'tVojvhToWjQ8Xvo4UPx2Xz9eRy7auyYMmZBjc2XfN' length = 31 bytes, must be 32"
      )
      d.appendBlockE(invoke(secondAddress, payments = Seq(Payment(1, IssuedAsset(ByteStr.fill(33)(1)))))) should produce(
        "invalid asset ID 'JJEfe6DcPM2ziB2vfUWDV6aHVerXRGkv3TcyvJUNGHZz' length = 33 bytes, must be 32"
      )
      d.appendBlockE(invoke(secondAddress, payments = Seq(Payment(1, IssuedAsset(ByteStr.fill(32)(1)))))) should produce(
        "asset '4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi' is not found on the blockchain"
      )
      val invokeWithIssued = invoke(secondAddress, payments = Seq(Payment(1, asset)))
      d.appendBlockE(invokeWithIssued) should produce(s"leads to negative asset '$asset' balance")
      d.appendBlock(transfer(secondSigner, defaultAddress, asset = asset))
      d.appendAndAssertSucceed(invokeWithIssued)
    }
  }

  property("ethereum invoke script transaction") {
    withDomain(
      TransactionStateSnapshot,
      AddrWithBalance.enoughBalances(defaultSigner, secondSigner) ++ Seq(
        AddrWithBalance(defaultSigner.toEthWavesAddress),
        AddrWithBalance(secondSigner.toEthWavesAddress)
      )
    ) { d =>
      d.appendBlock(setScript(secondSigner, dApp), issueTx, transfer(secondSigner, defaultSigner.toEthWavesAddress, asset = asset))
      Try(
        generateEthInvoke(defaultEthSigner, secondAddress, "default", Nil, Seq(Payment(1, IssuedAsset(ByteStr.fill(31)(1)))))
      ).toEither should produce("InvocationTargetException")
      Try(
        generateEthInvoke(defaultEthSigner, secondAddress, "default", Nil, Seq(Payment(1, IssuedAsset(ByteStr.fill(33)(1)))))
      ).toEither should produce("InvocationTargetException")
      d.appendBlockE(
        generateEthInvoke(defaultEthSigner, secondAddress, "default", Nil, Seq(Payment(1, IssuedAsset(ByteStr.fill(32)(1)))))
      ) should produce(
        "asset '4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi' is not found on the blockchain"
      )
      val invokeWithIssued = generateEthInvoke(secondSigner.toEthKeyPair, secondAddress, "default", Nil, payments = Seq(Payment(1, asset)))
      d.appendBlockE(invokeWithIssued) should produce("negative asset balance")
      d.appendBlock(transfer(secondSigner, secondSigner.toEthWavesAddress, asset = asset))
      d.appendAndAssertSucceed(invokeWithIssued)
    }
  }

  property("transfer transaction") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(secondSigner, dApp), issueTx)
      d.appendBlockE(transfer(asset = IssuedAsset(ByteStr.fill(31)(1)))) should produce(
        "invalid asset ID 'tVojvhToWjQ8Xvo4UPx2Xz9eRy7auyYMmZBjc2XfN' length = 31 bytes, must be 32"
      )
      d.appendBlockE(transfer(asset = IssuedAsset(ByteStr.fill(33)(1)))) should produce(
        "invalid asset ID 'JJEfe6DcPM2ziB2vfUWDV6aHVerXRGkv3TcyvJUNGHZz' length = 33 bytes, must be 32"
      )
      d.appendBlockE(transfer(asset = IssuedAsset(ByteStr.fill(32)(1)))) should produce(
        "asset '4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi' is not found on the blockchain"
      )
      val transferIssued = transfer(asset = asset)
      d.appendBlockE(transferIssued) should produce(s"leads to negative asset '$asset' balance")
      d.appendBlock(transfer(secondSigner, defaultAddress, asset = asset))
      d.appendAndAssertSucceed(transferIssued)
    }
  }

  property("ethereum transfer transaction") {
    withDomain(
      TransactionStateSnapshot,
      AddrWithBalance.enoughBalances(defaultSigner, secondSigner) ++ Seq(
        AddrWithBalance(defaultSigner.toEthWavesAddress),
        AddrWithBalance(secondSigner.toEthWavesAddress)
      )
    ) { d =>
      d.appendBlock(setScript(secondSigner, dApp), issueTx, transfer(secondSigner, defaultSigner.toEthWavesAddress, asset = asset))
      (31 to 33).foreach(i =>
        d.appendBlockE(generateEthTransfer(defaultEthSigner, secondAddress, 1, IssuedAsset(ByteStr.fill(i)(1)))) should produce(
          "Can't resolve ERC20 address"
        )
      )
      val transferIssued = generateEthTransfer(secondSigner.toEthKeyPair, secondAddress, 1, asset)
      d.appendBlockE(transferIssued) should produce(s"negative asset balance")
      d.appendBlock(transfer(secondSigner, secondSigner.toEthWavesAddress, asset = asset))
      d.appendAndAssertSucceed(transferIssued)
    }
  }
}
