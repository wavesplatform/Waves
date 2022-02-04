package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.{TestValues, TransactionGenBase}

class InvokeDataEntriesBytesTest extends PropSpec with WithDomain with TransactionGenBase {
  private def data(i: Int, size: Int): String =
    s"""
       | [BinaryEntry("key$i", base64'${ByteStr.fill(size / 2)(1)}' + base64'${ByteStr.fill(size / 2)(1)}')]
     """.stripMargin

  private def dApp1Script(dApp2: Address, size: Int, sync: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${if (sync) s"""strict r = Address(base58'$dApp2').invoke("default", [], [])""" else ""}
         |    ${data(1, size)}
         | }
       """.stripMargin
    )

  private def dApp2Script(dApp3: Address, size: Int): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = Address(base58'$dApp3').invoke("default", [], [])
         |    ${data(2, size)}
         | }
       """.stripMargin
    )

  private def dApp3Script(dApp4: Address, size: Int): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   strict r = Address(base58'$dApp4').invoke("default", [], [])
         |   ${data(3, size)}
         | }
       """.stripMargin
    )

  private def dApp4Script(size: Int, saveData: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = ${if (saveData) data(4, size) else "[]"}
       """.stripMargin
    )

  private def scenario(exceed5Kb: Boolean, sync: Boolean, reach15kb: Boolean = false) = {
    val invoker  = TxHelpers.signer(0)
    val dApp1    = TxHelpers.signer(1)
    val dApp2    = TxHelpers.signer(2)
    val dApp3    = TxHelpers.signer(3)
    val dApp4    = TxHelpers.signer(4)
    val gTx1     = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx2     = TxHelpers.genesis(dApp1.toAddress, TestValues.fee)
    val gTx3     = TxHelpers.genesis(dApp2.toAddress, ENOUGH_AMT)
    val gTx4     = TxHelpers.genesis(dApp3.toAddress, ENOUGH_AMT)
    val gTx5     = TxHelpers.genesis(dApp4.toAddress, ENOUGH_AMT)
    val limit    = ContractLimits.MaxWriteSetSizeInBytes - 9
    val size     = if (exceed5Kb) limit + 1 else limit
    val ssTx1    = TxHelpers.setScript(dApp1, dApp1Script(dApp2.toAddress, size, sync))
    val ssTx2    = TxHelpers.setScript(dApp2, dApp2Script(dApp3.toAddress, size))
    val ssTx3    = TxHelpers.setScript(dApp3, dApp3Script(dApp4.toAddress, size))
    val ssTx4    = TxHelpers.setScript(dApp4, dApp4Script(size, !reach15kb))
    val invokeTx = () => TxHelpers.invoke(dApp1.toAddress, None, Nil)
    (Seq(gTx1, gTx2, gTx3, gTx4, gTx5, ssTx1, ssTx2, ssTx3, ssTx4), invokeTx)
  }

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(checkTotalDataEntriesBytesHeight = 3, syncDAppCheckTransfersHeight = 4)

  property("exceeding 5 Kb before and after activation") {
    withDomain(domainSettingsWithFS(settings)) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = true, sync = true)
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      (the[RuntimeException] thrownBy d.appendBlock(invoke1)).getMessage should include(
        s"WriteSet size can't exceed 5120 bytes, actual: 5121 bytes"
      )

      d.appendBlock()
      d.appendBlock()
      val invoke2 = invoke()
      (the[RuntimeException] thrownBy d.appendBlock(invoke2)).getMessage should include(
        s"WriteSet size can't exceed 5120 bytes, actual: 5121 bytes"
      )
    }
  }

  property("exceeding 15 Kb before activation, after checkTotalDataEntriesBytesHeight and after syncDAppCheckTransfersHeight") {
    withDomain(domainSettingsWithFS(settings)) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = true)
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true

      val invoke2 = invoke()
      d.appendBlock(invoke2)
      d.blockchain.bestLiquidDiff.get.errorMessage(invoke2.id.value()).get.text should include(
        "Storing data size should not exceed 15360, actual: 20476 bytes"
      )

      val invoke3 = invoke()
      (the[Exception] thrownBy d.appendBlock(invoke3)).getMessage should include(
        "Storing data size should not exceed 15360, actual: 20476 bytes"
      )
    }
  }

  property("reaching 5 Kb before and after activation") {
    withDomain(domainSettingsWithFS(settings)) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = false)
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true

      val invoke2 = invoke()
      d.appendBlock(invoke2)
      d.blockchain.transactionSucceeded(invoke2.id.value()) shouldBe true
    }
  }

  property("reaching 15 Kb after activation") {
    withDomain(domainSettingsWithFS(settings)) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = true, reach15kb = true)
      d.appendBlock(preparingTxs: _*)
      d.appendBlock()

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true
    }
  }
}
