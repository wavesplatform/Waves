package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.util.AddressOrAliasExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produceRejectOrFailedDiff}
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, invoke, lease, secondAddress, secondSigner, setScript}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Authorized, CreateAliasTransaction, Transaction, TransactionType, TxHelpers, TxVersion}
import org.scalatest.exceptions.TestFailedException

import scala.util.Random

class LeaseActionDiffTest extends PropSpec with WithDomain {
  private def features(version: StdLibVersion): FunctionalitySettings = {
    val features = DomainPresets.settingsForRide(version).blockchainSettings.functionalitySettings.preActivatedFeatures
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = features)
  }

  private val v4Features = features(V4)
  private val v5Features = features(V5)

  private val setScriptFee = FeeConstants(TransactionType.SetScript) * FeeUnit
  private val leaseFee     = FeeConstants(TransactionType.Lease) * FeeUnit

  private def dApp(body: String, version: StdLibVersion = V5): Script =
    TestCompiler(version).compileContract(s"""
                                             | {-# STDLIB_VERSION ${version.id} #-}
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

  private def multipleLeaseCancelsDApp(leaseIds: Seq[ByteStr], version: StdLibVersion): Script =
    dApp(
      s"""
         | [
         |   ${leaseIds.map(id => s"LeaseCancel(base58'$id')").mkString(", ")}
         | ]
       """.stripMargin,
      version
    )

  private def multipleActionsDApp(
      recipient: Recipient,
      amount: Long,
      leaseCount: Int,
      leaseCancelCount: Int,
      transfersCount: Int,
      version: StdLibVersion
  ): Script =
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
       """.stripMargin,
      version
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
      customDApp: Option[Script] = None,
      version: StdLibVersion = V5
  ): (Seq[Transaction], InvokeScriptTransaction, Long, Address, Address, List[LeaseTransaction], LeaseCancelTransaction) = {
    val dAppAcc = TxHelpers.signer(1)
    val invoker = TxHelpers.signer(2)

    val invokerAlias   = Alias.create("invoker_alias").explicitGet()
    val dAppAlias      = Alias.create("dapp_alias").explicitGet()
    val fee            = TxHelpers.ciFee(1)
    val leaseTxAmount1 = 5.waves
    val leaseTxAmount2 = 10.waves
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
    val recipient   = customRecipient.getOrElse(generatedRecipient.toRide)
    val leaseAmount = customAmount.getOrElse(2.waves)

    val genesis = Seq(
      TxHelpers.genesis(dAppAcc.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val invoke        = TxHelpers.invoke(dAppAcc.toAddress, func = None, invoker = invoker, fee = fee, version = TxVersion.V1)
    val invokeAliasTx = TxHelpers.createAlias(invokerAlias.name, invoker, fee)
    val dAppAliasTx   = TxHelpers.createAlias(dAppAlias.name, dAppAcc, fee)
    val aliasTxs =
      if (useAlias)
        if (selfLease)
          Seq(invokeAliasTx, dAppAliasTx)
        else
          Seq(invokeAliasTx)
      else
        Seq.empty[CreateAliasTransaction]

    val leasesFromDApp = (1 to leaseCancelCount).toList.map(_ => TxHelpers.lease(dAppAcc, invoker.toAddress, leaseTxAmount1, fee))
    val setScript = TxHelpers.setScript(
      acc = dAppAcc,
      script = if (useLeaseCancelDApp) {
        multipleLeaseCancelsDApp(leasesFromDApp.map(_.id()), version)
      } else {
        customDApp.getOrElse(singleLeaseDApp(recipient, leaseAmount))
      },
      fee = customSetScriptFee.getOrElse(fee)
    )
    val preparingTxs = genesis ++ aliasTxs ++ Seq(setScript)

    val leaseToDApp = TxHelpers.lease(invoker, dAppAcc.toAddress, leaseTxAmount2, fee)
    val leaseCancelId =
      if (cancelLeaseActionByTx)
        Lease.calculateId(Lease(recipient, leaseAmount, 0), invoke.id())
      else if (cancelLeaseFromInvoker)
        leaseToDApp.id()
      else
        leasesFromDApp.head.id()
    val leaseCancelAcc = if (cancelLeaseFromInvoker) invoker else dAppAcc
    val leaseCancel    = TxHelpers.leaseCancel(leaseCancelId, sender = leaseCancelAcc, fee = fee)
    val leaseTxs       = leasesFromDApp :+ leaseToDApp

    (preparingTxs, invoke, leaseAmount, dAppAcc.toAddress, invoker.toAddress, leaseTxs, leaseCancel)
  }

  property(s"Lease action is restricted before activation ${BlockchainFeatures.SynchronousCalls}") {
    val (preparingTxs, _, _, _, _, _, _) = leasePreconditions()
    assertDiffEi(
      Seq(),
      TestBlock.create(preparingTxs),
      v4Features
    )(_ should produce("Ride V5, dApp-to-dApp invocations feature has not been activated yet"))
  }

  property(s"Lease action by address (invoker - recipient)") {

    val (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, _, _) = leasePreconditions()
    withDomain(domainSettingsWithFS(v5Features)) { d =>
      d.appendBlock(preparingTxs*)
      d.appendBlock(invoke)

      val invokerSpentFee  = preparingTxs.collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
      val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
      invokerPortfolio.lease shouldBe LeaseBalance(leaseAmount, out = 0)
      invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
      invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
      invokerPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + leaseAmount

      val dAppSpentFee  = preparingTxs.collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum
      val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
      dAppPortfolio.lease shouldBe LeaseBalance(in = 0, leaseAmount)
      dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
      dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
      dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount

      d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + leaseAmount
      d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
      d.appendBlock()
      d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee
      d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
    }
  }

  property(s"Lease action with active lease from dApp") {
    leasePreconditions() match {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: _, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = preparingTxs.collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val dAppSpentFee    = (preparingTxs :+ leaseTxFromDApp).collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs*)
          d.appendBlock(leaseTxFromDApp)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxFromDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseTxFromDApp.amount.value

          d.appendBlock(invoke)
          val totalLeaseAmount = leaseAmount + leaseTxFromDApp.amount.value

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(totalLeaseAmount, out = 0)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + totalLeaseAmount

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = 0, totalLeaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
        }
      case other => throw new TestFailedException(s"Unexpected preconditions $other", 0)
    }
  }

  property(s"Lease action with active lease from dApp and cancelled lease from invoker-recipient") {
    leasePreconditions(cancelLeaseFromInvoker = true) match {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: leaseTxToDApp :: Nil, leaseTxToDAppCancel) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee =
            (preparingTxs ++ Seq(leaseTxToDApp, leaseTxToDAppCancel)).collect {
              case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2
            }.sum
          val dAppSpentFee = (preparingTxs :+ leaseTxFromDApp).collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs*)
          d.appendBlock(leaseTxFromDApp, leaseTxToDApp, leaseTxToDAppCancel)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxFromDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseTxFromDApp.amount.value

          d.appendBlock(invoke)
          val totalLeaseAmount = leaseAmount + leaseTxFromDApp.amount.value

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(totalLeaseAmount, out = 0)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + totalLeaseAmount

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = 0, totalLeaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee.value + leaseTxToDAppCancel.fee.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee.value + leaseTxToDAppCancel.fee.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - totalLeaseAmount
        }
      case other => throw new TestFailedException(s"Unexpected preconditions $other", 0)
    }
  }

  property(s"Lease action with active lease from invoker-recipient") {
    leasePreconditions() match {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, _ :: leaseTxToDApp :: Nil, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = (preparingTxs :+ leaseTxToDApp).collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val dAppSpentFee    = preparingTxs.collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs*)
          d.appendBlock(leaseTxToDApp)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxToDApp.amount.value

          d.appendBlock(invoke)
          val leaseAmountDiff = leaseAmount - leaseTxToDApp.amount.value

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(in = leaseAmount, out = leaseTxToDApp.amount.value)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value - leaseTxToDApp.amount.value
          invokerPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + leaseAmountDiff

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = leaseTxToDApp.amount.value, out = leaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
          dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(0)
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(0)
        }
      case other => throw new TestFailedException(s"Unexpected preconditions $other", 0)
    }
  }

  property(s"Lease action with active lease from invoker-recipient and cancelled lease from dApp") {
    leasePreconditions() match {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: leaseTxToDApp :: Nil, leaseTxFromDAppCancel) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = (preparingTxs :+ leaseTxToDApp).collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val dAppSpentFee =
            (preparingTxs ++ Seq(leaseTxFromDApp, leaseTxFromDAppCancel)).collect {
              case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2
            }.sum

          d.appendBlock(preparingTxs*)
          d.appendBlock(leaseTxToDApp, leaseTxFromDApp, leaseTxFromDAppCancel)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxToDApp.amount.value

          d.appendBlock(invoke)
          val leaseAmountDiff = leaseAmount - leaseTxToDApp.amount.value

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(in = leaseAmount, out = leaseTxToDApp.amount.value)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value - leaseTxToDApp.amount.value
          invokerPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + leaseAmountDiff

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = leaseTxToDApp.amount.value, out = leaseAmount)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount
          dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(
            -leaseTxFromDApp.fee.value - leaseTxFromDAppCancel.fee.value
          )
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff.max(
            -leaseTxFromDApp.fee.value - leaseTxFromDAppCancel.fee.value
          )
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action with active lease from both dApp and invoker-recipient") {
    leasePreconditions() match {
      case (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, leaseTxFromDApp :: leaseTxToDApp :: Nil, _) =>
        withDomain(domainSettingsWithFS(v5Features)) { d =>
          val invokerSpentFee = (preparingTxs :+ leaseTxToDApp).collect { case a: Authorized if a.sender.toAddress == invoker => a.assetFee._2 }.sum
          val dAppSpentFee    = (preparingTxs :+ leaseTxFromDApp).collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum

          d.appendBlock(preparingTxs*)
          d.appendBlock(leaseTxFromDApp, leaseTxToDApp)

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee - leaseTxToDApp.amount.value + leaseTxFromDApp.amount.value
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxToDApp.amount.value - leaseTxFromDApp.amount.value

          d.appendBlock(invoke)
          val leaseAmountDiff = leaseAmount - leaseTxToDApp.amount.value + leaseTxFromDApp.amount.value

          val invokerPortfolio = d.blockchain.wavesPortfolio(invoker)
          invokerPortfolio.lease shouldBe LeaseBalance(in = leaseAmount + leaseTxFromDApp.amount.value, out = leaseTxToDApp.amount.value)
          invokerPortfolio.balance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value
          invokerPortfolio.spendableBalance shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value - leaseTxToDApp.amount.value
          invokerPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - invokerSpentFee - invoke.fee.value + leaseAmountDiff

          val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
          dAppPortfolio.lease shouldBe LeaseBalance(in = leaseTxToDApp.amount.value, out = leaseAmount + leaseTxFromDApp.amount.value)
          dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
          dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmount - leaseTxFromDApp.amount.value
          dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee - leaseAmountDiff

          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee.value.min(
            leaseTxFromDApp.amount.value - leaseTxToDApp.amount.value
          )
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxFromDApp.fee.value.min(-leaseAmountDiff)
          d.appendBlock()
          d.blockchain.generatingBalance(invoker) shouldBe ENOUGH_AMT - invokerSpentFee + leaseTxToDApp.fee.value.min(
            leaseTxFromDApp.amount.value - leaseTxToDApp.amount.value
          )
          d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee + leaseTxFromDApp.fee.value.min(-leaseAmountDiff)
        }
      case a => throw new TestFailedException(s"Unexpected preconditions $a", 0)
    }
  }

  property(s"Lease action by alias") {
    val (preparingTxs, invoke, leaseAmount, dAppAcc, invoker, _, _) = leasePreconditions(useAlias = true)
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value, LeaseBalance(leaseAmount, out = 0))
      diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(in = 0, leaseAmount))
    }
  }

  property(s"Lease action cancelled by LeaseCancelTransaction") {
    val (preparingTxs, invoke, _, dAppAcc, invoker, _, leaseCancelTx) = leasePreconditions(cancelLeaseActionByTx = true)
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke, leaseCancelTx)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()) shouldBe empty
      diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value)
      diff.portfolios(dAppAcc) shouldBe Portfolio(-leaseCancelTx.fee.value)
    }
  }

  property(s"Lease action with empty address") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customRecipient = Some(Recipient.Address(ByteStr.empty)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "InvalidAddress(Wrong addressBytes length: expected: 26, actual: 0)"
    }
  }

  property(s"Lease action with wrong address bytes length") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customRecipient = Some(Recipient.Address(ByteStr.fill(10)(127))))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "InvalidAddress(Wrong addressBytes length: expected: 26, actual: 10)"
    }
  }

  property(s"Lease action with wrong address checksum") {
    val address                               = TxHelpers.signer(3).toAddress
    val wrongChecksum                         = Array.fill[Byte](Address.ChecksumLength)(0)
    val wrongAddress                          = address.bytes.dropRight(Address.ChecksumLength) ++ wrongChecksum
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customRecipient = Some(Recipient.Address(ByteStr(wrongAddress))))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "InvalidAddress(Bad address checksum)"
    }
  }

  property(s"Lease action with unexisting alias") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customRecipient = Some(Recipient.Alias("alias2")))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "Alias 'alias:T:alias2' does not exist."
    }
  }

  property(s"Lease action with illegal alias") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customRecipient = Some(Recipient.Alias("#$%!?")))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe s"Alias should contain only following characters: ${Alias.AliasAlphabet}"
    }
  }

  property(s"Lease action with empty amount") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customAmount = Some(0))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "NonPositiveAmount(0,waves)"
    }
  }

  property(s"Lease action with negative amount") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customAmount = Some(-100))
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    )(_ should produce("Negative lease amount"))
  }

  property(s"Lease action spends all dApp balance") {
    val setScriptFee                                = TxHelpers.ciFee(1)
    val dAppBalance                                 = ENOUGH_AMT - setScriptFee
    val (preparingTxs, invoke, _, dAppAcc, _, _, _) = leasePreconditions(customSetScriptFee = Some(setScriptFee), customAmount = Some(dAppBalance))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (_, blockchain) =>
      blockchain.wavesPortfolio(dAppAcc).effectiveBalance(false).explicitGet() shouldBe 0
    }
  }

  property(s"Lease action on insufficient balance") {
    val setScriptFee                          = TxHelpers.ciFee(1)
    val dAppBalance                           = ENOUGH_AMT - setScriptFee
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customSetScriptFee = Some(setScriptFee), customAmount = Some(dAppBalance + 1))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe s"Cannot lease more than own: Balance: $dAppBalance, already leased: 0"
    }
  }

  property(s"Lease action on insufficient balance with other leases") {
    val setScriptFee                                 = TxHelpers.ciFee(1)
    val dAppBalance                                  = ENOUGH_AMT - setScriptFee
    val (preparingTxs, invoke, _, _, _, leaseTxs, _) = leasePreconditions(customSetScriptFee = Some(setScriptFee), customAmount = Some(dAppBalance))
    val leaseFromDApp                                = leaseTxs.head
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs.toList ::: leaseTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe
        s"Cannot lease more than own: Balance: ${dAppBalance - leaseFromDApp.fee.value}, already leased: ${leaseFromDApp.amount.value}"
    }
  }

  property(s"Duplicate lease action") {
    val recipient                             = TxHelpers.signer(3).toAddress.toRide
    val amount                                = positiveLongGen.sample.get
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(duplicateLeaseDApp(recipient, amount)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      val id = Lease.calculateId(Lease(recipient, amount, nonce = 0), invoke.id())
      diff.errorMessage(invoke.id()).get.text shouldBe s"Lease with id=$id is already in the state"
    }
  }

  property(s"Lease action to dApp itself") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(selfLease = true)
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "Cannot lease to self"
    }
  }

  property(s"Lease action to dApp itself by alias") {
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(selfLease = true, useAlias = true)
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "Cannot lease to self"
    }
  }

  Seq(V5, V6).foreach { version =>
    val limit =
      if (version == V6)
        ContractLimits.MaxBalanceScriptActionsAmountV6
      else
        ContractLimits.MaxCallableActionsAmountBeforeV6(version)
    property(s"$limit Lease actions for V${version.id}") {
      val recipient = TxHelpers.signer(3).toAddress
      val amount    = 100
      val dApp = multipleActionsDApp(
        recipient.toRide,
        amount,
        leaseCount = limit,
        leaseCancelCount = 0,
        transfersCount = 0,
        version = version
      )
      val (preparingTxs, invoke, _, dAppAcc, invoker, _, _) = leasePreconditions(customDApp = Some(dApp))
      assertDiffAndState(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features(version)
      ) { case (diff, _) =>
        diff.errorMessage(invoke.id()) shouldBe empty
        diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value)
        diff.portfolios(dAppAcc) shouldBe Portfolio(lease = LeaseBalance(in = 0, out = amount * limit))
        diff.portfolios(recipient) shouldBe Portfolio(lease = LeaseBalance(in = amount * limit, out = 0))
      }
    }

    property(s"${limit + 1} Lease actions for V${version.id}") {
      val recipient = TxHelpers.signer(3).toAddress
      val amount    = 100
      val dApp = multipleActionsDApp(
        recipient.toRide,
        amount,
        leaseCount = limit + 1,
        leaseCancelCount = 0,
        transfersCount = 0,
        version = version
      )
      val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(dApp))

      val errMsg =
        if (version == V6)
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"
        else
          "Actions count limit is exceeded"

      assertDiffEi(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features(version)
      )(_ should produceRejectOrFailedDiff(errMsg))
    }
  }

  property(s"LeaseCancel action with Lease action from same result") {
    val recipient                                   = TxHelpers.signer(3).toAddress
    val amount                                      = 100
    val (preparingTxs, invoke, _, dAppAcc, _, _, _) = leasePreconditions(customDApp = Some(leaseWithLeaseCancelDApp(recipient.toRide, amount)))
    withDomain(domainSettingsWithFS(v5Features)) { d =>
      d.appendBlock(preparingTxs*)
      d.appendBlock(invoke)

      val recipientPortfolio = d.blockchain.wavesPortfolio(recipient)
      recipientPortfolio.lease shouldBe LeaseBalance.empty
      recipientPortfolio.balance shouldBe 0
      recipientPortfolio.spendableBalance shouldBe 0
      recipientPortfolio.effectiveBalance(false).explicitGet() shouldBe 0

      val dAppSpentFee  = preparingTxs.collect { case a: Authorized if a.sender.toAddress == dAppAcc => a.assetFee._2 }.sum
      val dAppPortfolio = d.blockchain.wavesPortfolio(dAppAcc)
      dAppPortfolio.lease shouldBe LeaseBalance.empty
      dAppPortfolio.balance shouldBe ENOUGH_AMT - dAppSpentFee
      dAppPortfolio.spendableBalance shouldBe ENOUGH_AMT - dAppSpentFee
      dAppPortfolio.effectiveBalance(false).explicitGet() shouldBe ENOUGH_AMT - dAppSpentFee

      d.blockchain.generatingBalance(recipient) shouldBe 0
      d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee
      d.appendBlock()
      d.blockchain.generatingBalance(recipient) shouldBe 0
      d.blockchain.generatingBalance(dAppAcc) shouldBe ENOUGH_AMT - dAppSpentFee
    }
  }

  property(s"LeaseCancel action between two same Lease actions") {
    val recipient                             = TxHelpers.signer(3).toAddress
    val amount                                = 100
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(leaseAfterLeaseCancelDApp(recipient.toRide, amount)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text should include("is already in the state")
    }
  }

  property(s"LeaseCancel action between two Lease actions with different nonces") {
    val recipient = TxHelpers.signer(3).toAddress
    val amount    = 100
    val (preparingTxs, invoke, _, dAppAcc, invoker, _, _) =
      leasePreconditions(customDApp = Some(differentLeaseAfterLeaseCancelDApp(recipient.toRide, amount)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()) shouldBe empty
      diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value)
      diff.portfolios(dAppAcc) shouldBe Portfolio(lease = LeaseBalance(in = 0, out = amount))
      diff.portfolios(recipient) shouldBe Portfolio(lease = LeaseBalance(in = amount, 0))
    }
  }

  property(s"LeaseCancel action for lease performed via LeaseTransaction") {
    val (preparingTxs, invoke, _, dAppAcc, invoker, leaseTxs, _) = leasePreconditions(useLeaseCancelDApp = true)
    val leaseFromDApp                                            = leaseTxs.head
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs ++ leaseTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value, LeaseBalance(in = -leaseFromDApp.amount.value, 0))
      diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(0, out = -leaseFromDApp.amount.value))
    }
  }

  property(s"LeaseCancel action with unexisting leaseId") {
    val leaseId                               = ByteStr.fill(32)(1.toByte)
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(singleLeaseCancelDApp(leaseId)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe s"Lease with id=$leaseId not found"
    }
  }

  property(s"LeaseCancel action with illegal leaseId") {
    val leaseId                               = ByteStr.fromBytes(1)
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(singleLeaseCancelDApp(leaseId)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe s"Lease id=$leaseId has invalid length = 1 byte(s) while expecting 32"
    }
  }

  property("LeaseCancel foreign leaseId") {
    val leaseTx                               = TxHelpers.lease(TxHelpers.signer(2))
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(singleLeaseCancelDApp(leaseTx.id())))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs :+ leaseTx)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text should include("LeaseTransaction was leased by other sender")
    }
  }

  property(s"LeaseCancel actions with same lease id") {
    val recipient                             = TxHelpers.signer(3).toAddress.toRide
    val amount                                = 100
    val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(duplicatedLeaseCancelDApp(recipient, amount)))
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      val leaseId = Lease.calculateId(Lease(recipient, amount, nonce = 0), invoke.id())
      diff.errorMessage(invoke.id()).get.text shouldBe s"Duplicate LeaseCancel id(s): $leaseId"
    }
  }

  property(s"LeaseCancel action for already cancelled lease") {
    val (preparingTxs, invoke, _, _, _, leaseTxs, leaseCancelTx) = leasePreconditions(useLeaseCancelDApp = true)
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs ++ leaseTxs ++ List(leaseCancelTx))),
      TestBlock.create(Seq(invoke)),
      v5Features
    ) { case (diff, _) =>
      diff.errorMessage(invoke.id()).get.text shouldBe "Cannot cancel already cancelled lease"
    }
  }

  Seq(V5, V6).foreach { version =>
    property(s"${ContractLimits.MaxCallableActionsAmountBeforeV6(version)} LeaseCancel actions for V${version.id}") {
      val (preparingTxs, invoke, _, dAppAcc, invoker, ltx, _) =
        leasePreconditions(useLeaseCancelDApp = true, leaseCancelCount = ContractLimits.MaxCallableActionsAmountBeforeV6(version), version = version)
      val leaseTxs = ltx.init
      assertDiffAndState(
        Seq(TestBlock.create(preparingTxs ++ leaseTxs)),
        TestBlock.create(Seq(invoke)),
        features(version)
      ) { case (diff, _) =>
        diff.errorMessage(invoke.id()) shouldBe empty
        diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value, LeaseBalance(in = -leaseTxs.map(_.amount.value).sum, out = 0))
        diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(in = 0, out = -leaseTxs.map(_.amount.value).sum))
      }
    }

    property(s"${ContractLimits.MaxCallableActionsAmountBeforeV6(version) + 1} LeaseCancel actions for V${version.id}") {
      val (preparingTxs, invoke, _, _, _, ltx, _) =
        leasePreconditions(useLeaseCancelDApp = true, leaseCancelCount = ContractLimits.MaxCallableActionsAmountBeforeV6(version) + 1)
      assertDiffEi(
        Seq(TestBlock.create(preparingTxs ++ ltx.init)),
        TestBlock.create(Seq(invoke)),
        features(version)
      )(_ should produceRejectOrFailedDiff("Actions count limit is exceeded"))
    }
  }

  Seq(V5, V6).foreach { version =>
    val limit =
      if (version == V6)
        ContractLimits.MaxBalanceScriptActionsAmountV6
      else
        ContractLimits.MaxCallableActionsAmountBeforeV6(version)
    property(s"$limit multiple balance actions for V${version.id}") {
      val recipient        = accountGen.sample.get.toAddress
      val amount           = positiveLongGen.sample.get
      val actionsCount     = limit
      val leaseCount       = Random.nextInt(actionsCount) + 1
      val leaseCancelCount = Random.nextInt(leaseCount).min(actionsCount - leaseCount)
      val transfersCount   = actionsCount - leaseCancelCount - leaseCount
      val dApp = multipleActionsDApp(
        recipient.toRide,
        amount,
        leaseCount,
        leaseCancelCount,
        transfersCount,
        version
      )
      val leaseAmount                                       = (leaseCount - leaseCancelCount) * amount
      val (preparingTxs, invoke, _, dAppAcc, invoker, _, _) = leasePreconditions(customDApp = Some(dApp))
      assertDiffAndState(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features(version)
      ) { case (diff, _) =>
        diff.errorMessage(invoke.id()) shouldBe empty
        diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee.value)
        diff.portfolios(dAppAcc) shouldBe Portfolio(-transfersCount, lease = LeaseBalance(in = 0, out = leaseAmount))
        diff.portfolios(recipient) shouldBe Portfolio(transfersCount, lease = LeaseBalance(in = leaseAmount, out = 0))
      }
    }

    property(s"${limit + 1} multiple balance actions for V${version.id}") {
      val recipient         = TxHelpers.signer(3).toAddress
      val amount            = 100
      val totalActionsCount = limit + 1
      val leaseCount        = Random.nextInt(totalActionsCount) + 1
      val leaseCancelCount  = Random.nextInt(leaseCount).min(totalActionsCount - leaseCount)
      val transfersCount    = totalActionsCount - leaseCancelCount - leaseCount
      val dApp = multipleActionsDApp(
        recipient.toRide,
        amount,
        leaseCount,
        leaseCancelCount,
        transfersCount,
        version
      )
      val (preparingTxs, invoke, _, _, _, _, _) = leasePreconditions(customDApp = Some(dApp))

      val errMsg =
        if (version == V6)
          "ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded"
        else
          "Actions count limit is exceeded"

      assertDiffEi(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features(version)
      )(_ should produceRejectOrFailedDiff(errMsg))
    }
  }

  property("trying to spend lease IN balance in Lease action") {
    withDomain(RideV5, Seq(AddrWithBalance(secondAddress, setScriptFee))) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   [
           |     Lease(i.caller, 1)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(lease(recipient = secondAddress, amount = 1))
      d.appendAndAssertFailed(invoke(), "Cannot lease more than own: Balance: 0, already leased: 0")
    }
  }

  property("trying to spend lease OUT balance in Lease action") {
    withDomain(
      RideV5,
      Seq(AddrWithBalance(secondAddress, leaseFee + setScriptFee + 1))
    ) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   [
           |     Lease(i.caller, 1)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(lease(sender = secondSigner, recipient = defaultAddress, amount = 1))
      d.appendAndAssertFailed(invoke(), "Cannot lease more than own: Balance: 1, already leased: 1")
    }
  }

  property("ScriptTransfer after Lease of all available balance") {
    val setScriptFee = FeeConstants(TransactionType.SetScript) * FeeUnit
    withDomain(
      RideV5.configure(_.copy(blockVersion3AfterHeight = 0)),
      Seq(AddrWithBalance(secondAddress, setScriptFee + 1))
    ) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   [
           |     Lease(i.caller, 1),
           |     ScriptTransfer(i.caller, 1, unit)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlockE(invoke()) should produce("negative effective balance")
    }
  }

  property("ScriptTransfer after LeaseCancel of transferring balance") {
    withDomain(
      RideV5.configure(_.copy(blockVersion3AfterHeight = 0)),
      Seq(AddrWithBalance(secondAddress, 1.006.waves))
    ) { d =>
      val leaseTx = lease(secondSigner, defaultAddress, amount = 1)
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   [
           |     LeaseCancel(base58'${leaseTx.id()}'),
           |     ScriptTransfer(i.caller, 1, unit)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp), leaseTx)
      d.appendAndAssertSucceed(invoke())
    }
  }
}
