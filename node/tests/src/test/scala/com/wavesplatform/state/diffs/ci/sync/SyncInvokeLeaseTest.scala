package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.TransactionType
import com.wavesplatform.transaction.TxHelpers.*
import org.scalatest.OptionValues

class SyncInvokeLeaseTest extends PropSpec with WithDomain with OptionValues {
  import DomainPresets.*

  private val dApp1Signer  = secondSigner
  private val dApp1Address = secondAddress
  private val dApp2Signer  = signer(2)
  private val dApp2Address = signer(2).toAddress

  private def twoLeaseDApp(secondAmount: Int) = TestCompiler(V5).compileContract(
    s"""
       | @Callable(i)
       | func default() = {
       |   strict leaseId = this.invoke("lease", [], [])
       |   [
       |     LeaseCancel(leaseId.exactAs[ByteVector]),
       |     Lease(Address(base58'$dApp2Address'), $secondAmount)
       |   ]
       | }
       |
       | @Callable(i)
       | func lease() = {
       |   let lease = Lease(Address(base58'$dApp2Address'), 1)
       |   let id    = calculateLeaseId(lease)
       |   ([lease], id)
       | }
     """.stripMargin
  )

  property("LeaseCancel for Lease from sync call current dApp and then new Lease") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer)) { d =>
      d.appendBlock(setScript(dApp1Signer, twoLeaseDApp(555)))
      d.appendAndAssertSucceed(invoke(dApp1Address))
      d.liquidSnapshot.cancelledLeases.size shouldBe 1
      val cancelledLeaseId = d.liquidSnapshot.cancelledLeases.head._1
      val cancelledLease   = d.liquidSnapshot.newLeases.get(cancelledLeaseId).value
      cancelledLease.recipientAddress shouldBe dApp2Address
      cancelledLease.amount.value shouldBe 1
      val lease2 = d.liquidSnapshot.newLeases.removed(cancelledLeaseId).head._2
      lease2.recipientAddress shouldBe dApp2Address
      lease2.amount.value shouldBe 555
    }
  }

  property("LeaseCancel for Lease from sync call current dApp and then new Lease with previous id") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer)) { d =>
      d.appendBlock(setScript(dApp1Signer, twoLeaseDApp(1)))
      d.appendAndAssertFailed(invoke(dApp1Address), "already in the state")
      d.liquidSnapshot.newLeases should be(empty)
      d.liquidSnapshot.cancelledLeases should be(empty)
    }
  }

  property("double LeaseCancel for Lease from sync call current dApp") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict leaseId = this.invoke("lease", [], [])
           |   [
           |     LeaseCancel(leaseId.exactAs[ByteVector])
           |   ]
           | }
           |
           | @Callable(i)
           | func lease() = {
           |   let lease = Lease(Address(base58'$dApp2Address'), 1)
           |   let id    = calculateLeaseId(lease)
           |   ([lease, LeaseCancel(id)], id)
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp))
      d.appendAndAssertFailed(invoke(dApp1Address), "Cannot cancel already cancelled lease")
      d.liquidSnapshot.newLeases should be(empty)
      d.liquidSnapshot.cancelledLeases should be(empty)
    }
  }

  property("LeaseCancel foreign Lease") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict leaseId = Address(base58'$dApp2Address').invoke("lease", [], [])
           |   [
           |     LeaseCancel(leaseId.exactAs[ByteVector])
           |   ]
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func lease() = {
           |   let lease = Lease(i.caller, 1)
           |   let id    = calculateLeaseId(lease)
           |   ([lease], id)
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertFailed(invoke(dApp1Address), "LeaseTransaction was leased by other sender and")
    }
  }

  property("Lease with the same id as foreign Lease") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("lease", [], [])
           |   [
           |     Lease(Address(base58'$defaultAddress'), 1)
           |   ]
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func lease() =
           |   [
           |     Lease(Address(base58'$defaultAddress'), 1)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertFailed(invoke(dApp1Address), "already in the state")
    }
  }

  property("Lease ahead of corresponding LeaseCancel") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict lease = this.invoke("lease", [], [])
           |   [lease.exactAs[Lease]]
           | }
           |
           | @Callable(i)
           | func lease() = {
           |   let lease = Lease(Address(base58'$dApp2Address'), 1)
           |   let id    = calculateLeaseId(lease)
           |   ([LeaseCancel(id)], lease)
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp))
      d.appendBlockE(invoke(dApp1Address)) should produce("not found")
    }
  }

  property("Lease after Lease with whole balance") {
    val setScriptFee = FeeConstants(TransactionType.SetScript) * FeeUnit
    withDomain(RideV5, Seq(AddrWithBalance(dApp1Address, setScriptFee + 2))) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = this.invoke("lease", [], [])
           |   [
           |     Lease(Address(base58'$dApp2Address'), 1)
           |   ]
           | }
           |
           | @Callable(i)
           | func lease() = {
           |   [
           |     Lease(Address(base58'$dApp2Address'), 2)
           |   ]
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp))
      d.appendAndAssertFailed(invoke(dApp1Address), "Cannot lease more than own: Balance: 2, already leased: 2")
    }
  }
}
