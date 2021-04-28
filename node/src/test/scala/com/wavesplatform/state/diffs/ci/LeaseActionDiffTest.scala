package com.wavesplatform.state.diffs.ci

import cats.implicits._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.util.AddressOrAliasExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Authorized, GenesisTransaction, Transaction}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class LeaseActionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDomain with EitherValues {
  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def features(activateV5: Boolean): FunctionalitySettings = {
    val v5ForkO = if (activateV5) Seq(BlockchainFeatures.SynchronousCalls) else Seq()
    val parameters =
      Seq(
        BlockchainFeatures.SmartAccounts,
        BlockchainFeatures.SmartAssets,
        BlockchainFeatures.Ride4DApps,
        BlockchainFeatures.BlockV5
      ) ++ v5ForkO
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = parameters.map(_.id -> 0).toMap)
  }

  private val v4Features = features(activateV5 = false)
  private val v5Features = features(activateV5 = true)

  private def dApp(body: String): Script = TestCompiler(V5).compileContract(s"""
    | {-# STDLIB_VERSION 5       #-}
    | {-# CONTENT_TYPE   DAPP    #-}
    | {-# SCRIPT_TYPE    ACCOUNT #-}
    |
    | @Callable(i)
    | func default() = {
    |   $body
    | }
    |""".stripMargin)

  private def singleLeaseDApp(recipient: Recipient, amount: Long): Script =
    dApp(s"[Lease(${recipientStr(recipient)}, $amount)]")

  private def duplicateLeaseDApp(recipient: Recipient, amount: Long): Script =
    dApp(
      s"""
         | [
         |   Lease(${recipientStr(recipient)}, $amount),
         |   Lease(${recipientStr(recipient)}, $amount)
         | ]
       """.stripMargin
    )

  private def singleLeaseCancelDApp(leaseId: ByteStr): Script =
    dApp(s"[LeaseCancel(base58'$leaseId')]")

  private def leaseWithLeaseCancelDApp(recipient: Recipient, amount: Long): Script =
    dApp(
      s"""
         | let lease = Lease(${recipientStr(recipient)}, $amount)
         | let id    = calculateLeaseId(lease)
         | [
         |   lease,
         |   LeaseCancel(id)
         | ]
       """.stripMargin
    )

  private def leaseAfterLeaseCancelDApp(recipient: Recipient, amount: Long): Script =
    dApp(
      s"""
         | let lease = Lease(${recipientStr(recipient)}, $amount)
         | let id    = calculateLeaseId(lease)
         | [
         |   lease,
         |   LeaseCancel(id),
         |   lease
         | ]
       """.stripMargin
    )

  private def differentLeaseAfterLeaseCancelDApp(recipient: Recipient, amount: Long): Script =
    dApp(
      s"""
         | let lease = Lease(${recipientStr(recipient)}, $amount)
         | let id    = calculateLeaseId(lease)
         | [
         |   lease,
         |   LeaseCancel(id),
         |   Lease(${recipientStr(recipient)}, $amount, 1)
         | ]
       """.stripMargin
    )

  private def duplicatedLeaseCancelDApp(recipient: Recipient, amount: Long): Script =
    dApp(
      s"""
         | let lease = Lease(${recipientStr(recipient)}, $amount)
         | let id    = calculateLeaseId(lease)
         | [
         |   lease,
         |   LeaseCancel(id),
         |   LeaseCancel(id)
         | ]
       """.stripMargin
    )

  private def multipleLeaseCancelsDApp(leaseIds: Seq[ByteStr]): Script =
    dApp(
      s"""
         | [
         |   ${leaseIds.map(id => s"LeaseCancel(base58'$id')").mkString(", ")}
         | ]
       """.stripMargin
    )

  private def multipleActionsDApp(recipient: Recipient, amount: Long, leaseCount: Int, leaseCancelCount: Int, transfersCount: Int): Script =
    dApp(
      s"""
         | ${(1 to leaseCount).map(i => s"let lease$i = Lease(${recipientStr(recipient)}, $amount, $i)").mkString("\n")}
         | [
         |   ${(1 to leaseCount).map(i => s"lease$i").mkString(", ")}
         |   ${if (leaseCancelCount > 0) "," else ""}
         |   ${(1 to leaseCancelCount).map(i => s"LeaseCancel(calculateLeaseId(lease$i))").mkString(", ")}
         |   ${if (transfersCount > 0) "," else ""}
         |   ${(1 to transfersCount).map(_ => s"ScriptTransfer(${recipientStr(recipient)}, 1, unit)").mkString(", ")}
         | ]
       """.stripMargin
    )

  private def recipientStr(recipient: Recipient) =
    recipient match {
      case Recipient.Address(bytes) => s"Address(base58'$bytes')"
      case Recipient.Alias(name)    => s"""Alias("$name")"""
    }

  private def leasePreconditions(
      useAlias: Boolean = false,
      selfLease: Boolean = false,
      useLeaseCancelDApp: Boolean = false,
      leaseCancelCount: Int = 1,
      cancelLeaseActionByTx: Boolean = false,
      cancelLeaseFromInvoker: Boolean = false,
      customRecipient: Option[Recipient] = None,
      customAmount: Option[Long] = None,
      customSetScriptFee: Option[Long] = None,
      customDApp: Option[Script] = None
  ): Gen[(List[Transaction], InvokeScriptTransaction, Long, Address, Address, List[LeaseTransaction], LeaseCancelTransaction)] =
    for {
      dAppAcc         <- accountGen
      invoker         <- accountGen
      generatedAmount <- positiveLongGen
      fee             <- ciFee(1)
      leaseTxAmount1  <- positiveLongGen
      leaseTxAmount2  <- positiveLongGen
      invokerAlias = Alias.create("invoker_alias").explicitGet()
      dAppAlias    = Alias.create("dapp_alias").explicitGet()
      invokeAliasTx <- createAliasGen(invoker, invokerAlias, fee, ts)
      dAppAliasTx   <- createAliasGen(dAppAcc, dAppAlias, fee, ts)
    } yield {
      val aliasTxs =
        if (useAlias)
          if (selfLease)
            List(invokeAliasTx, dAppAliasTx)
          else
            List(invokeAliasTx)
        else
          Nil
      val generatedRecipient =
        if (selfLease)
          if (useAlias)
            dAppAlias
          else
            dAppAcc.toAddress
        else if (useAlias)
          invokerAlias
        else
          invoker.toAddress
      val recipient    = customRecipient.getOrElse(generatedRecipient.toRide)
      val leaseAmount  = customAmount.getOrElse(generatedAmount)
      val setScriptFee = customSetScriptFee.getOrElse(fee)
      for {
        genesis  <- GenesisTransaction.create(dAppAcc.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        invoke   <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, dAppAcc.toAddress, None, Nil, fee, Waves, ts)
        leasesFromDApp <- (1 to leaseCancelCount).toList.traverse(
          i => LeaseTransaction.selfSigned(2.toByte, dAppAcc, invoker.toAddress, leaseTxAmount1, fee, ts + i)
        )
        leaseToDApp <- LeaseTransaction.selfSigned(2.toByte, invoker, dAppAcc.toAddress, leaseTxAmount2, fee, ts + 100)
        calculatedId = Lease.calculateId(Lease(recipient, leaseAmount, 0), invoke.id.value())
        leaseCancelId = if (cancelLeaseActionByTx) calculatedId
        else if (cancelLeaseFromInvoker) leaseToDApp.id.value()
        else leasesFromDApp.head.id.value()
        leaseCancelAcc = if (cancelLeaseFromInvoker) invoker else dAppAcc
        leaseCancel <- LeaseCancelTransaction.signed(2.toByte, leaseCancelAcc.publicKey, leaseCancelId, fee, ts + 100, leaseCancelAcc.privateKey)
        multipleCancelDApp = multipleLeaseCancelsDApp(leasesFromDApp.map(_.id.value()))
        dApp               = if (useLeaseCancelDApp) multipleCancelDApp else customDApp.getOrElse(singleLeaseDApp(recipient, leaseAmount))
        setDApp <- SetScriptTransaction.selfSigned(1.toByte, dAppAcc, Some(dApp), setScriptFee, ts + 100)
        preparingTxs = List(genesis, genesis2) ::: aliasTxs ::: List(setDApp)
        leaseTxs     = leasesFromDApp :+ leaseToDApp
      } yield (preparingTxs, invoke, leaseAmount, dAppAcc.toAddress, invoker.toAddress, leaseTxs, leaseCancel)
    }.explicitGet()

  property(s"Lease action is restricted before activation ${BlockchainFeatures.SynchronousCalls}") {
    forAll(leasePreconditions()) {
      case (preparingTxs, _, _, _, _, _, _) =>
          assertDiffEi(
            Seq(),
            TestBlock.create(preparingTxs),
            v4Features
          )(_ should produce("Ride V5, dApp-to-dApp invocations feature has not been activated yet"))
    }
  }

  property(s"Lease action by address (invoker - recipient)") {
    forAll(leasePreconditions()) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, _, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          d.appendBlock(preparingTxs: _*)
          d.appendBlock(invoke)

          val invokerSpentFee  = preparingTxs.collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(leaseAmount, out = 0)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.effectiveBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + leaseAmount

          val dAppSpentFee  = preparingTxs.collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum
          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = 0, leaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + leaseAmount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
        }
    }
  }

  property(s"Lease action with active lease from dApp") {
    forAll(leasePreconditions()) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: _, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = preparingTxs.collect { case a: Authorized if a.sender.toAddress == invoker                      => a.assetFee._2 }.sum
          val dAppSpentFee    = (preparingTxs :+ leaseTxFromDApp).collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs: _*)
          d.appendBlock(leaseTxFromDApp)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxFromDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseTxFromDApp.amount

          d.appendBlock(invoke)
          val totalLeaseAmount = leaseAmount + leaseTxFromDApp.amount

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(totalLeaseAmount, out = 0)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.effectiveBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + totalLeaseAmount

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = 0, totalLeaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action with active lease from dApp and cancelled lease from invoker-recipient") {
    forAll(leasePreconditions(cancelLeaseFromInvoker = true)) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: leaseTxToDApp :: Nil, leaseTxToDAppCancel) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee =
            (preparingTxs ++ Seq(leaseTxToDApp, leaseTxToDAppCancel))
              .collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2}
              .sum
          val dAppSpentFee = (preparingTxs :+ leaseTxFromDApp).collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs: _*)
          d.appendBlock(leaseTxFromDApp, leaseTxToDApp, leaseTxToDAppCancel)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxFromDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseTxFromDApp.amount

          d.appendBlock(invoke)
          val totalLeaseAmount = leaseAmount + leaseTxFromDApp.amount

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(totalLeaseAmount, out = 0)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.effectiveBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + totalLeaseAmount

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = 0, totalLeaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee + leaseTxToDAppCancel.fee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee + leaseTxToDAppCancel.fee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action with active lease from invoker-recipient") {
    forAll(leasePreconditions()) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, _ :: leaseTxToDApp :: Nil, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = (preparingTxs :+ leaseTxToDApp).collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val dAppSpentFee    = preparingTxs.collect { case a: Authorized if a.sender.toAddress == dAppAcc                    => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs: _*)
          d.appendBlock(leaseTxToDApp)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxToDApp.amount

          d.appendBlock(invoke)
          val leaseAmountDiff = leaseAmount - leaseTxToDApp.amount

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(in = leaseAmount, out = leaseTxToDApp.amount)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee - leaseTxToDApp.amount
          invokerPortfolio.effectiveBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + leaseAmountDiff

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = leaseTxToDApp.amount, out = leaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(0)
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(0)
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action with active lease from invoker-recipient and cancelled lease from dApp") {
    forAll(leasePreconditions()) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: leaseTxToDApp :: Nil, leaseTxFromDAppCancel) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = (preparingTxs :+ leaseTxToDApp).collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val dAppSpentFee =
            (preparingTxs ++ Seq(leaseTxFromDApp, leaseTxFromDAppCancel))
              .collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2}
              .sum

          d.appendBlock(preparingTxs: _*)
          d.appendBlock(leaseTxToDApp, leaseTxFromDApp, leaseTxFromDAppCancel)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxToDApp.amount

          d.appendBlock(invoke)
          val leaseAmountDiff = leaseAmount - leaseTxToDApp.amount

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(in = leaseAmount, out = leaseTxToDApp.amount)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee - leaseTxToDApp.amount
          invokerPortfolio.effectiveBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + leaseAmountDiff

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = leaseTxToDApp.amount, out = leaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(-leaseTxFromDApp.fee - leaseTxFromDAppCancel.fee)
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(-leaseTxFromDApp.fee - leaseTxFromDAppCancel.fee)
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action with active lease from both dApp and invoker-recipient") {
    forAll(leasePreconditions()) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: leaseTxToDApp :: Nil, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = (preparingTxs :+ leaseTxToDApp).collect { case a: Authorized if a.sender.toAddress == invoker   => a.assetFee._2 }.sum
          val dAppSpentFee    = (preparingTxs :+ leaseTxFromDApp).collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs: _*)
          d.appendBlock(leaseTxFromDApp, leaseTxToDApp)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount + leaseTxFromDApp.amount
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxToDApp.amount - leaseTxFromDApp.amount

          d.appendBlock(invoke)
          val leaseAmountDiff = leaseAmount - leaseTxToDApp.amount + leaseTxFromDApp.amount

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(in = leaseAmount + leaseTxFromDApp.amount, out = leaseTxToDApp.amount)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee - leaseTxToDApp.amount
          invokerPortfolio.effectiveBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee + leaseAmountDiff

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = leaseTxToDApp.amount, out = leaseAmount + leaseTxFromDApp.amount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount - leaseTxFromDApp.amount
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee.min(
            leaseTxFromDApp.amount - leaseTxToDApp.amount
          )
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxFromDApp.fee.min(-leaseAmountDiff)
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee.min(
            leaseTxFromDApp.amount - leaseTxToDApp.amount
          )
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxFromDApp.fee.min(-leaseAmountDiff)
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action by alias") {
    forAll(leasePreconditions(useAlias = true)) {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee, LeaseBalance(leaseAmount, out = 0))
            diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(in = 0, leaseAmount))
        }
    }
  }

  property(s"Lease action cancelled by LeaseCancelTransaction") {
    forAll(leasePreconditions(cancelLeaseActionByTx = true)) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, _, leaseCancelTx) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke, leaseCancelTx)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee)
            diff.portfolios(dAppAcc) shouldBe Portfolio(-leaseCancelTx.fee)
        }
    }
  }

  property(s"Lease action with empty address") {
    forAll(leasePreconditions(customRecipient = Some(Recipient.Address(ByteStr.empty)))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "InvalidAddress(Wrong addressBytes length: expected: 26, actual: 0)"
        }
    }
  }

  property(s"Lease action with wrong address bytes length") {
    forAll(leasePreconditions(customRecipient = Some(Recipient.Address(ByteStr.fill(10)(127))))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "InvalidAddress(Wrong addressBytes length: expected: 26, actual: 10)"
        }
    }
  }

  property(s"Lease action with wrong address checksum") {
    val address       = accountGen.sample.get.toAddress
    val wrongChecksum = Array.fill[Byte](Address.ChecksumLength)(0)
    val wrongAddress  = address.bytes.dropRight(Address.ChecksumLength) ++ wrongChecksum
    forAll(leasePreconditions(customRecipient = Some(Recipient.Address(ByteStr(wrongAddress))))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "InvalidAddress(Bad address checksum)"
        }
    }
  }

  property(s"Lease action with unexisting alias") {
    forAll(leasePreconditions(customRecipient = Some(Recipient.Alias("alias2")))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Alias 'alias:T:alias2' does not exists."
        }
    }
  }

  property(s"Lease action with illegal alias") {
    forAll(leasePreconditions(customRecipient = Some(Recipient.Alias("#$%!?")))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Alias should contain only following characters: ${Alias.AliasAlphabet}"
        }
    }
  }

  property(s"Lease action with empty amount") {
    forAll(leasePreconditions(customAmount = Some(0))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "NonPositiveAmount(0,waves)"
        }
    }
  }

  property(s"Lease action with negative amount") {
    forAll(leasePreconditions(customAmount = Some(-100))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "NonPositiveAmount(-100,waves)"
        }
    }
  }

  property(s"Lease action spends all dApp balance") {
    val setScriptFee = ciFee(1).sample.get
    val dAppBalance  = ENOUGH_AMT - setScriptFee
    forAll(leasePreconditions(customSetScriptFee = Some(setScriptFee), customAmount = Some(dAppBalance))) {
      case (preparingTxs, invoke, _, dAppAcc, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (_, blockchain) =>
            blockchain.wavesPortfolio(dAppAcc).effectiveBalance shouldBe 0
        }
    }
  }

  property(s"Lease action on insufficient balance") {
    val setScriptFee = ciFee(1).sample.get
    val dAppBalance  = ENOUGH_AMT - setScriptFee
    forAll(leasePreconditions(customSetScriptFee = Some(setScriptFee), customAmount = Some(dAppBalance + 1))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Cannot lease more than own: Balance: $dAppBalance, already leased: 0"
        }
    }
  }

  property(s"Lease action on insufficient balance with other leases") {
    val setScriptFee = ciFee(1).sample.get
    val dAppBalance  = ENOUGH_AMT - setScriptFee
    forAll(leasePreconditions(customSetScriptFee = Some(setScriptFee), customAmount = Some(dAppBalance))) {
      case (preparingTxs, invoke, _, _, _, leaseTxs @ List(leaseFromDApp, _), _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs ::: leaseTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe
              s"Cannot lease more than own: Balance: ${dAppBalance - leaseFromDApp.fee}, already leased: ${leaseFromDApp.amount}"
        }
    }
  }

  property(s"Duplicate lease action") {
    val recipient = accountGen.sample.get.toAddress.toRide
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(duplicateLeaseDApp(recipient, amount)))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            val id = Lease.calculateId(Lease(recipient, amount, nonce = 0), invoke.id.value())
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Lease with id=$id is already in the state"
        }
    }
  }

  property(s"Lease action to dApp itself") {
    forAll(leasePreconditions(selfLease = true)) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Cannot lease to self"
        }
    }
  }

  property(s"Lease action to dApp itself by alias") {
    forAll(leasePreconditions(selfLease = true, useAlias = true)) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Cannot lease to self"
        }
    }
  }

  property(s"10 Lease actions") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    val dApp      = multipleActionsDApp(recipient.toRide, amount, leaseCount = 10, leaseCancelCount = 0, transfersCount = 0)
    forAll(leasePreconditions(customDApp = Some(dApp))) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee)
            diff.portfolios(dAppAcc) shouldBe Portfolio(lease = LeaseBalance(in = 0, out = amount * 10))
            diff.portfolios(recipient) shouldBe Portfolio(lease = LeaseBalance(in = amount * 10, out = 0))
        }
    }
  }

  property(s"31 Lease actions") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    val dApp      = multipleActionsDApp(recipient.toRide, amount, leaseCount = 31, leaseCancelCount = 0, transfersCount = 0)
    forAll(leasePreconditions(customDApp = Some(dApp))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Actions count limit is exceeded"
        }
    }
  }

  property(s"LeaseCancel action with Lease action from same result") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(leaseWithLeaseCancelDApp(recipient.toRide, amount)))) {
      case (preparingTxs, invoke, _, dAppAcc, _, _, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          d.appendBlock(preparingTxs: _*)
          d.appendBlock(invoke)

          val recipientPortfolio = d.blockchain.wavesPortfolio(recipient)
          recipientPortfolio.lease shouldBe LeaseBalance.empty
          recipientPortfolio.balance shouldBe 0
          recipientPortfolio.spendableBalance shouldBe 0
          recipientPortfolio.effectiveBalance shouldBe 0

          val dAppSpentFee  = preparingTxs.collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum
          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance.empty
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.effectiveBalance shouldBe ENOUGH_AMT - dAppSpentFee

          d.blockchain.generatingBalance(recipient) shouldBe 0
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee
          d.appendBlock()
          d.blockchain.generatingBalance(recipient) shouldBe 0
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee
        }
    }
  }

  property(s"LeaseCancel action between two same Lease actions") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(leaseAfterLeaseCancelDApp(recipient.toRide, amount)))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text should include("is already in the state")
        }
    }
  }

  property(s"LeaseCancel action between two Lease actions with different nonces") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(differentLeaseAfterLeaseCancelDApp(recipient.toRide, amount)))) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee)
            diff.portfolios(dAppAcc) shouldBe Portfolio(lease = LeaseBalance(in = 0, out = amount))
            diff.portfolios(recipient) shouldBe Portfolio(lease = LeaseBalance(in = amount, 0))
        }
    }
  }

  property(s"LeaseCancel action for lease performed via LeaseTransaction") {
    forAll(leasePreconditions(useLeaseCancelDApp = true)) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, leaseTxs @ List(leaseFromDApp, _), _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs ++ leaseTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee, LeaseBalance(in = -leaseFromDApp.amount, 0))
            diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(0, out = -leaseFromDApp.amount))
        }
    }
  }

  property(s"LeaseCancel action with unexisting leaseId") {
    val leaseId = attachmentGen.sample.get
    forAll(leasePreconditions(customDApp = Some(singleLeaseCancelDApp(leaseId)))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Lease with id=$leaseId not found"
        }
    }
  }

  property(s"LeaseCancel action with illegal leaseId") {
    val leaseId = ByteStr.fromBytes(1)
    forAll(leasePreconditions(customDApp = Some(singleLeaseCancelDApp(leaseId)))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Lease id=$leaseId has invalid length = 1 byte(s) while expecting 32"
        }
    }
  }

  property(s"LeaseCancel actions with same lease id") {
    val recipient = accountGen.sample.get.toAddress.toRide
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(duplicatedLeaseCancelDApp(recipient, amount)))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            val leaseId = Lease.calculateId(Lease(recipient, amount, nonce = 0), invoke.id.value())
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Duplicate LeaseCancel id(s): $leaseId"
        }
    }
  }

  property(s"LeaseCancel action for already cancelled lease") {
    forAll(leasePreconditions(useLeaseCancelDApp = true)) {
      case (preparingTxs, invoke, _, _, _, leaseTxs, leaseCancelTx) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs ++ leaseTxs ++ List(leaseCancelTx))),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Cannot cancel already cancelled lease"
        }
    }
  }

  property(s"10 LeaseCancel actions") {
    forAll(leasePreconditions(useLeaseCancelDApp = true, leaseCancelCount = 10)) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, leaseTxs :+ _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs ++ leaseTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee, LeaseBalance(in = -leaseTxs.map(_.amount).sum, out = 0))
            diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(in = 0, out = -leaseTxs.map(_.amount).sum))
        }
    }
  }

  property(s"31 LeaseCancel actions") {
    forAll(leasePreconditions(useLeaseCancelDApp = true, leaseCancelCount = 31)) {
      case (preparingTxs, invoke, _, _, _, leaseTxs :+ _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs ++ leaseTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Actions count limit is exceeded"
        }
    }
  }

  property(s"10 multiple actions") {
    val recipient        = accountGen.sample.get.toAddress
    val amount           = positiveLongGen.sample.get
    val leaseCount       = Random.nextInt(10) + 1
    val leaseCancelCount = Random.nextInt(leaseCount).min(10 - leaseCount)
    val transfersCount   = 10 - leaseCancelCount - leaseCount
    val dApp             = multipleActionsDApp(recipient.toRide, amount, leaseCount, leaseCancelCount, transfersCount)
    val leaseAmount      = (leaseCount - leaseCancelCount) * amount
    forAll(leasePreconditions(customDApp = Some(dApp))) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee)
            diff.portfolios(dAppAcc) shouldBe Portfolio(-transfersCount, lease = LeaseBalance(in = 0, out = leaseAmount))
            diff.portfolios(recipient) shouldBe Portfolio(transfersCount, lease = LeaseBalance(in = leaseAmount, out = 0))
        }
    }
  }

  property(s"31 multiple actions") {
    val recipient        = accountGen.sample.get.toAddress
    val amount           = positiveLongGen.sample.get
    val leaseCount       = Random.nextInt(31) + 1
    val leaseCancelCount = Random.nextInt(leaseCount).min(31 - leaseCount)
    val transfersCount   = 31 - leaseCancelCount - leaseCount
    val dApp             = multipleActionsDApp(recipient.toRide, amount, leaseCount, leaseCancelCount, transfersCount)
    forAll(leasePreconditions(customDApp = Some(dApp))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Actions count limit is exceeded"
        }
    }
  }
}
