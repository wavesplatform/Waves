package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.{BooleanDataEntry, EmptyDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class IsDataStorageUntouchedTest extends PropSpec with WithDomain {

  import DomainPresets.*

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

  private val scenario = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val dataTx       = TxHelpers.dataV2(master, Seq(BooleanDataEntry("q", true)))
    val deleteDataTx = TxHelpers.dataV2(master, Seq(EmptyDataEntry("q")))
    val setScript    = TxHelpers.setScript(master, contract)
    val invoke       = TxHelpers.invoke(master.toAddress, invoker = invoker)

    (genesis :+ setScript, dataTx, deleteDataTx, invoke, master.toAddress)
  }

  property("isDataStorageUntouched true") {
    val (genesisTxs, _, _, invokeTx, dApp) = scenario
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invokeTx)
      d.blockchain.accountData(dApp, "virgin") shouldBe Some(BooleanDataEntry("virgin", true))
    }
  }

  property("isDataStorageUntouched false") {
    val (genesisTxs, dataTx, _, invokeTx, dApp) = scenario
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
    val (genesisTxs, dataTx, deleteDataTx, invokeTx, dApp) = scenario
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

    val scenario = {
      val dApp1 = TxHelpers.signer(0)
      val dApp2 = TxHelpers.signer(1)

      val genesis = Seq(
        TxHelpers.genesis(dApp1.toAddress),
        TxHelpers.genesis(dApp2.toAddress)
      )
      val setScript1 = TxHelpers.setScript(dApp1, syncDApp(dApp2.toAddress))
      val setScript2 = TxHelpers.setScript(dApp2, contract)

      val invoke = TxHelpers.invoke(dApp1.toAddress, invoker = dApp1, version = TxVersion.V1)

      (genesis :+ setScript1 :+ setScript2, invoke)
    }

    val (genesisTxs, invokeTx) = scenario
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs*).id()
      d.appendBlock(invokeTx)
      d.blockchain.accountData(invokeTx.dApp.asInstanceOf[Address], "start") shouldBe Some(BooleanDataEntry("start", true))
      d.blockchain.accountData(invokeTx.dApp.asInstanceOf[Address], "end") shouldBe Some(BooleanDataEntry("end", false))
    }
  }
}
