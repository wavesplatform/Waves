package com.wavesplatform.state.diffs.smart.predef
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.{BooleanDataEntry, EmptyDataEntry}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IsDataStorageUntouchedTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDomain
    with EitherValues {

  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val contract = TestCompiler(V5).compileContract(
    s"""
       |{-# STDLIB_VERSION 5       #-}
       |{-# CONTENT_TYPE   DAPP    #-}
       |{-#SCRIPT_TYPE     ACCOUNT #-}
       |
       | @Callable(i)
       | func default() = {
       |   let check = isDataStorageUntouched(this)
       |   [ BooleanEntry("virgin", check) ]
       | }
     """.stripMargin
  )

  private val scenario =
    for {
      master  <- accountGen
      invoker <- accountGen
      fee     <- ciFee()
      gTx1         = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2         = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      dataTx       = DataTransaction.selfSigned(TxVersion.V2, master, Seq(BooleanDataEntry("q", true)), 15000000, ts).explicitGet()
      deleteDataTx = DataTransaction.selfSigned(TxVersion.V2, master, Seq(EmptyDataEntry("q")), 15000000, ts).explicitGet()
      ssTx         = SetScriptTransaction.selfSigned(1.toByte, master, Some(contract), fee, ts).explicitGet()
      invokeTx     = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, master.toAddress, None, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, ssTx), dataTx, deleteDataTx, invokeTx, master.toAddress)

  property("isDataStorageUntouched true") {
    val (genesisTxs, _, _, invokeTx, dApp) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invokeTx)
      d.blockchain.bestLiquidDiff.get.errorMessage(invokeTx.id.value()) shouldBe None
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", true))
    }
  }

  property("isDataStorageUntouched false") {
    val (genesisTxs, dataTx, _, invokeTx, dApp) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(dataTx)
      d.appendBlock(invokeTx)
      d.blockchain.bestLiquidDiff.get.errorMessage(invokeTx.id.value()) shouldBe None
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", false))
    }
  }

  property("isDataStorageUntouched false after delete") {
    val (genesisTxs, dataTx, deleteDataTx, invokeTx, dApp) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(dataTx, deleteDataTx)
      d.appendBlock(invokeTx)
      d.blockchain.bestLiquidDiff.get.errorMessage(invokeTx.id.value()) shouldBe None
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", false))
    }
  }
}
