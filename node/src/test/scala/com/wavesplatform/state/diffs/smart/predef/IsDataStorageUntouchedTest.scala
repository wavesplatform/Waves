package com.wavesplatform.state.diffs.smart.predef
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.{BooleanDataEntry, EmptyDataEntry}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed

class IsDataStorageUntouchedTest extends PropSpec with WithDomain {

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
      invokeTx     = Signed.invokeScript(TxVersion.V3, invoker, master.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2, ssTx), dataTx, deleteDataTx, invokeTx, master.toAddress)

  property("isDataStorageUntouched true") {
    val (genesisTxs, _, _, invokeTx, dApp) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invokeTx)
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", true))
    }
  }

  property("isDataStorageUntouched false") {
    val (genesisTxs, dataTx, _, invokeTx, dApp) = scenario.sample.get
    withDomain(RideV5) { d =>
      val genesis = d.appendBlock(genesisTxs*).id()

      d.appendBlock(dataTx)
      d.appendBlock(invokeTx)
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", false))

      d.rollbackTo(genesis)
      d.appendBlock(invokeTx)
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", true))
    }
  }

  property("isDataStorageUntouched false after delete") {
    val (genesisTxs, dataTx, deleteDataTx, invokeTx, dApp) = scenario.sample.get
    withDomain(RideV5) { d =>
      val genesis = d.appendBlock(genesisTxs*).id()

      d.appendBlock(dataTx, deleteDataTx)
      d.appendBlock(invokeTx)
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", false))

      d.rollbackTo(genesis)
      d.appendBlock(invokeTx)
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", true))
    }
  }

  property("isDataStorageUntouched in sync dApp") {
    def syncDApp(nextDApp: Address): Script = TestCompiler(V5).compileContract(
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() = {
         |   let address  = Address(base58'$nextDApp')
         |   strict start = isDataStorageUntouched(address)
         |   strict r     = invoke(address, "default", [], [])
         |   strict end   = isDataStorageUntouched(address)
         |   [
         |     BooleanEntry("start", start),
         |     BooleanEntry("end", end)
         |   ]
         | }
       """.stripMargin
    )

    val scenario =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(syncDApp(dApp2.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(contract), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp1.toAddress, None, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, setDApp1, setDApp2), invoke)

    val (genesisTxs, invokeTx) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs*).id()
      d.appendBlock(invokeTx)
      d.blockchain.accountData(invokeTx.dApp.asInstanceOf[Address], "start") shouldBe Some(BooleanDataEntry("start", true))
      d.blockchain.accountData(invokeTx.dApp.asInstanceOf[Address], "end") shouldBe Some(BooleanDataEntry("end", false))
    }
  }
}
