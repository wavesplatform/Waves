package com.wavesplatform.state.diffs.ci

import cats.implicits._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Random

class LeaseActionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDomain with EitherValues {
  private def features(activateV5: Boolean): FunctionalitySettings = {
    val v5ForkO = if (activateV5) Seq(BlockchainFeatures.ContinuationTransaction) else Seq()
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

  private def dApp(body: String): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() = {
         |   $body
         | }
       """.stripMargin

    val expr     = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, V5)
    ContractScript(V5, contract).explicitGet()
  }

  private def singleLeaseDApp(recipient: Recipient, amount: Long): Script =
    dApp(s"[Lease(${recipientStr(recipient)}, $amount)]")

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
      customRecipient: Option[Recipient] = None,
      customAmount: Option[Long] = None,
      customSetScriptFee: Option[Long] = None,
      customDApp: Option[Script] = None
  ): Gen[(List[Transaction], InvokeScriptTransaction, Long, Address, Address, List[LeaseTransaction], LeaseCancelTransaction)] =
    for {
      dAppAcc         <- accountGen
      invoker         <- accountGen
      generatedAmount <- positiveLongGen
      ts              <- timestampGen
      fee             <- ciFee(1)
      leaseTxAmount1  <- smallFeeGen
      leaseTxAmount2  <- smallFeeGen
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
        leaseCancelId = if (cancelLeaseActionByTx) Lease.calculateId(Lease(recipient, leaseAmount, 0), invoke.id.value())
        else leasesFromDApp.head.id.value()
        leaseCancel <- LeaseCancelTransaction.signed(2.toByte, dAppAcc.publicKey, leaseCancelId, fee, ts + 100, dAppAcc.privateKey)
        dApp = if (useLeaseCancelDApp) multipleLeaseCancelsDApp(leasesFromDApp.map(_.id.value()))
        else customDApp.getOrElse(singleLeaseDApp(recipient, leaseAmount))
        setDApp <- SetScriptTransaction.selfSigned(1.toByte, dAppAcc, Some(dApp), setScriptFee, ts + 100)
        preparingTxs = List(genesis, genesis2) ::: aliasTxs ::: List(setDApp)
        leaseTxs     = leasesFromDApp :+ leaseToDApp
      } yield (preparingTxs, invoke, leaseAmount, dAppAcc.toAddress, invoker.toAddress, leaseTxs, leaseCancel)
    }.explicitGet()

  property(s"Lease action is restricted before activation ${BlockchainFeatures.ContinuationTransaction}") {
    forAll(leasePreconditions()) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        def r(): Unit =
          assertDiffEi(
            Seq(TestBlock.create(preparingTxs)),
            TestBlock.create(Seq(invoke)),
            v4Features
          )(_ => ())
        (the[RuntimeException] thrownBy r()).getMessage should include("Continuation Transaction feature has not been activated yet")
    }
  }

  property(s"Lease action by address") {
    forAll(leasePreconditions()) {
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
            diff.errorMessage(invoke.id.value()).get.text shouldBe s"Cannot lease more than own: Balance:$dAppBalance, already leased: 0"
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
              s"Cannot lease more than own: Balance:${dAppBalance - leaseFromDApp.fee}, already leased: ${leaseFromDApp.amount}"
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

  property(s"11 Lease actions") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    val dApp      = multipleActionsDApp(recipient.toRide, amount, leaseCount = 11, leaseCancelCount = 0, transfersCount = 0)
    forAll(leasePreconditions(customDApp = Some(dApp))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Too many script actions: max: 10, actual: 11"
        }
    }
  }

  property(s"LeaseCancel action with Lease action from same result") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(leaseWithLeaseCancelDApp(recipient.toRide, amount)))) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee)
            diff.portfolios(dAppAcc) shouldBe Portfolio()
            diff.portfolios(recipient) shouldBe Portfolio()
        }
    }
  }

  property(s"LeaseCancel action between two same Lease actions") {
    val recipient = accountGen.sample.get.toAddress
    val amount    = positiveLongGen.sample.get
    forAll(leasePreconditions(customDApp = Some(leaseAfterLeaseCancelDApp(recipient.toRide, amount)))) {
      case (preparingTxs, invoke, _, dAppAcc, invoker, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()) shouldBe empty
            diff.portfolios(invoker) shouldBe Portfolio(-invoke.fee)
            diff.portfolios(dAppAcc) shouldBe Portfolio(0, LeaseBalance(in = 0, out = amount))
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

  property(s"11 LeaseCancel actions") {
    forAll(leasePreconditions(useLeaseCancelDApp = true, leaseCancelCount = 11)) {
      case (preparingTxs, invoke, _, _, _, leaseTxs :+ _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs ++ leaseTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Too many script actions: max: 10, actual: 11"
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

  property(s"11 multiple actions") {
    val recipient        = accountGen.sample.get.toAddress
    val amount           = positiveLongGen.sample.get
    val leaseCount       = Random.nextInt(11) + 1
    val leaseCancelCount = Random.nextInt(leaseCount).min(11 - leaseCount)
    val transfersCount   = 11 - leaseCancelCount - leaseCount
    val dApp             = multipleActionsDApp(recipient.toRide, amount, leaseCount, leaseCancelCount, transfersCount)
    forAll(leasePreconditions(customDApp = Some(dApp))) {
      case (preparingTxs, invoke, _, _, _, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          v5Features
        ) {
          case (diff, _) =>
            diff.errorMessage(invoke.id.value()).get.text shouldBe "Too many script actions: max: 10, actual: 11"
        }
    }
  }
}
