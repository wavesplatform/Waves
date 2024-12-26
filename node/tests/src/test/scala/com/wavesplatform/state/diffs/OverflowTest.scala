package com.wavesplatform.state.diffs

import com.wavesplatform.TestValues
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.issue
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{TransactionType, TxHelpers}

class OverflowTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val transferFee     = FeeConstants(TransactionType.Transfer) * FeeUnit
  private val massTransferFee = 0.002.waves

  private def numPairs(fee: Long) =
    Seq(
      (Long.MaxValue, 1L),
      (1L + fee, Long.MaxValue - fee),
      (Long.MaxValue / 2 + 1, Long.MaxValue / 2 + 1)
    )

  private val sender      = TxHelpers.signer(1)
  private val recipient   = TxHelpers.signer(2).toAddress
  private val recipientKp = TxHelpers.signer(2)

  property("transfer overflow") {
    numPairs(transferFee).foreach { case (recipientBalance, transferAmount) =>
      val balances = Seq(AddrWithBalance(sender.toAddress, Long.MaxValue), AddrWithBalance(recipient, recipientBalance))
      withDomain(RideV5, balances) { d =>
        d.appendBlockE(TxHelpers.transfer(sender, recipient, transferAmount)) should produce("Waves balance sum overflow")
      }
    }
  }

  property("mass transfer overflow") {
    numPairs(massTransferFee).foreach { case (recipientBalance, transferAmount) =>
      val balances = Seq(AddrWithBalance(sender.toAddress, Long.MaxValue), AddrWithBalance(recipient, recipientBalance))
      withDomain(RideV5, balances) { d =>
        d.appendBlockE(TxHelpers.massTransfer(sender, Seq(recipient -> transferAmount), fee = massTransferFee)) should produce(
          "Waves balance sum overflow"
        )
      }
    }
  }

  property("mass transfer overflow in list of transfers") {
    numPairs(massTransferFee).foreach { case (balance1, balance2) =>
      (the[Exception] thrownBy TxHelpers.massTransfer(
        sender,
        Seq(
          recipient -> balance1,
          recipient -> balance2
        )
      )).getMessage shouldBe "OverflowError"
    }
  }

  property("invoke payments overflow") {
    val dApp = TestCompiler(V5).compileContract(
      """
        | @Callable(i)
        | func default() = []
     """.stripMargin
    )
    Seq(
      (Long.MaxValue, 1L + TestValues.fee),
      (1L + TestValues.fee, Long.MaxValue),
      (Long.MaxValue / 2 + 1, Long.MaxValue / 2 + 1 + TestValues.fee)
    ).foreach { case (recipientBalance, paymentAmount) =>
      val balances = Seq(AddrWithBalance(sender.toAddress, Long.MaxValue), AddrWithBalance(recipient, recipientBalance))
      withDomain(RideV5, balances) { d =>
        d.appendBlock(TxHelpers.setScript(recipientKp, dApp))
        d.appendBlockE(TxHelpers.invoke(recipient, invoker = sender, payments = Seq(Payment(paymentAmount, Waves)))) should produce(
          "Waves balance sum overflow"
        )
      }
    }
  }

  property("invoke transfers overflow") {
    def dApp(amount: Long) = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = [ ScriptTransfer(i.caller, $amount, unit) ]
       """.stripMargin
    )
    Seq(
      (Long.MaxValue, 1L + TestValues.fee),
      (1L + TestValues.fee * 2, Long.MaxValue - TestValues.fee),
      (Long.MaxValue / 2 + 1 + TestValues.fee, Long.MaxValue / 2 + 1)
    ).foreach { case (invokerBalance, transferAmount) =>
      val balances = Seq(AddrWithBalance(sender.toAddress, invokerBalance), AddrWithBalance(recipient, Long.MaxValue))
      withDomain(RideV5, balances) { d =>
        d.appendBlock(TxHelpers.setScript(recipientKp, dApp(transferAmount)))
        d.appendBlockE(TxHelpers.invoke(recipient, invoker = sender)) should produce("Waves balance sum overflow")
      }
    }
  }

  property("invoke ScriptTransfer overflow") {
    def dApp(amount1: Long, amount2: Long) = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() =
         |   [
         |     ScriptTransfer(i.caller, $amount1, unit),
         |     ScriptTransfer(i.caller, $amount2, unit)
         |   ]
       """.stripMargin
    )
    numPairs(0).foreach { case (amount1, amount2) =>
      withDomain(RideV5, AddrWithBalance.enoughBalances(sender, recipientKp)) { d =>
        d.appendBlock(TxHelpers.setScript(recipientKp, dApp(amount1, amount2)))
        val invoke = TxHelpers.invoke(recipient, invoker = sender)
        d.appendAndAssertFailed(invoke, "ScriptTransfer overflow")
      }
    }
  }

  property("invoke Reissue overflow") {
    numPairs(0).foreach { case (amount1, amount2) =>
      withDomain(RideV5, AddrWithBalance.enoughBalances(sender, recipientKp)) { d =>
        val issueTx = issue(recipientKp, amount = amount1)
        val asset   = IssuedAsset(issueTx.id())
        val dApp = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() =
             |   [
             |     Reissue(base58'$asset', $amount2, false)
             |   ]
             """.stripMargin
        )
        val invoke = TxHelpers.invoke(recipient, invoker = sender)
        d.appendBlock(issueTx, TxHelpers.setScript(recipientKp, dApp))
        d.appendAndAssertFailed(invoke, "Asset total value overflow")
      }
    }
  }
}
