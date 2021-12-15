package com.wavesplatform.state.diffs.ci

import com.wavesplatform.TransactionGenBase
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test._
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed

class InvokeDataEntriesBytesTest extends PropSpec with WithDomain with TransactionGenBase {
  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private def scenario(exceed5Kb: Boolean, sync: Boolean, reach15kb: Boolean = false) =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      dApp2   <- accountGen
      dApp3   <- accountGen
      dApp4   <- accountGen
      fee     <- ciFee()
      gTx1       = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2       = GenesisTransaction.create(dApp1.toAddress, fee, ts).explicitGet()
      gTx3       = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx4       = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx5       = GenesisTransaction.create(dApp4.toAddress, ENOUGH_AMT, ts).explicitGet()
      limit      = ContractLimits.MaxWriteSetSizeInBytes - 9
      wholeLimit = ContractLimits.MaxTotalWriteSetSizeInBytes - 9
      size       = if (exceed5Kb) limit + 1 else limit
      ssTx1      = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script(dApp2.toAddress, size, sync)), fee, ts).explicitGet()
      ssTx2      = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp2Script(dApp3.toAddress, size)), fee, ts).explicitGet()
      ssTx3      = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp3Script(dApp4.toAddress, size)), fee, ts).explicitGet()
      ssTx4      = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp4Script(size, !reach15kb)), fee, ts).explicitGet()
      invokeTx   = () => Signed.invokeScript(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2, gTx3, gTx4, gTx5, ssTx1, ssTx2, ssTx3, ssTx4), invokeTx)

  property("exceeding 5 Kb after activation") {
    withDomain(DomainPresets.RideV5) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = true, sync = true).sample.get
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      (the[RuntimeException] thrownBy d.appendBlock(invoke1)).getMessage should include(
        s"WriteSet size can't exceed 5120 bytes, actual: 5121 bytes"
      )
    }
  }

  property("exceeding 15 Kb after activation") {
    withDomain(DomainPresets.RideV5) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = true).sample.get
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      d.appendAndAssertFailed(invoke1)
    }
  }

  property("exceeding 15 Kb with RideV6") {
    withDomain(DomainPresets.RideV6) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = true).sample.get
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      d.appendAndCatchError(invoke1).toString should include("Storing data size should not exceed 15360")
    }
  }

  property("reaching 5 Kb after activation") {
    withDomain(DomainPresets.RideV5) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = false).sample.get
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true
    }
  }

  property("reaching 15 Kb after activation") {
    withDomain(DomainPresets.RideV5) { d =>
      val (preparingTxs, invoke) = scenario(exceed5Kb = false, sync = true, reach15kb = true).sample.get
      d.appendBlock(preparingTxs: _*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true
    }
  }
}
