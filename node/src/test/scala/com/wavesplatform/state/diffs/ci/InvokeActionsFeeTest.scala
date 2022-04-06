package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.SynchronousCalls
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Transaction, TransactionType, TxHelpers}
import org.scalatest.{EitherValues, Inside}

class InvokeActionsFeeTest extends PropSpec with Inside with WithState with DBCacheSettings with WithDomain with EitherValues {
  import DomainPresets.*

  private val activationHeight = 4
  private val fsWithV5 =
    RideV5
      .configure(_.copy(estimatorPreCheckHeight = Int.MaxValue))
      .setFeaturesHeight(SynchronousCalls -> activationHeight)

  private val verifier: Script =
    TestCompiler(V4).compileExpression(
      s""" {-# STDLIB_VERSION 4        #-}
         | {-# SCRIPT_TYPE ASSET       #-}
         | {-# CONTENT_TYPE EXPRESSION #-}
         |
         | !(sigVerify_32Kb(base58'', base58'', base58'') ||
         |   sigVerify_32Kb(base58'', base58'', base58'') ||
         |   sigVerify_32Kb(base58'', base58'', base58''))
       """.stripMargin
    )

  private def dApp(asset: IssuedAsset): Script =
    TestCompiler(V4).compileContract(s"""
         | @Callable(i)
         | func default() =
         |  [
         |     ScriptTransfer(i.caller, 1, base58'$asset'),
         |     Burn(base58'$asset', 1),
         |     Reissue(base58'$asset', 1, false)
         |  ]
      """.stripMargin)

  private val paymentPreconditions: (Seq[AddrWithBalance], List[Transaction], () => InvokeScriptTransaction, () => InvokeScriptTransaction) = {
    val dAppAcc               = TxHelpers.signer(0)
    val scriptedInvoker       = TxHelpers.signer(1)
    val nonScriptedInvoker    = TxHelpers.signer(2)
    val balances              = AddrWithBalance.enoughBalances(dAppAcc, scriptedInvoker, nonScriptedInvoker)
    val issue                 = TxHelpers.issue(dAppAcc, script = Some(verifier))
    val asset                 = IssuedAsset(issue.id())
    val transfer1             = TxHelpers.transfer(dAppAcc, scriptedInvoker.toAddress, 10, asset)
    val transfer2             = TxHelpers.transfer(dAppAcc, nonScriptedInvoker.toAddress, 10, asset)
    val setVerifier           = TxHelpers.setScript(scriptedInvoker, verifier)
    val setDApp               = TxHelpers.setScript(dAppAcc, dApp(asset))
    val payments              = Seq(Payment(1, asset), Payment(1, asset))
    val invokeFromScripted    = () => TxHelpers.invoke(dAppAcc.toAddress, None, Nil, payments, scriptedInvoker)
    val invokeFromNonScripted = () => TxHelpers.invoke(dAppAcc.toAddress, None, Nil, payments, nonScriptedInvoker)
    (balances, List(issue, transfer1, transfer2, setVerifier, setDApp), invokeFromScripted, invokeFromNonScripted)
  }

  property(s"fee for asset scripts is not required after activation ${BlockchainFeatures.SynchronousCalls}") {
    val (balances, preparingTxs, invokeFromScripted, invokeFromNonScripted) = paymentPreconditions
    withDomain(fsWithV5, balances) { d =>
      d.appendBlock(preparingTxs*)

      val invokeFromScripted1    = invokeFromScripted()
      val invokeFromNonScripted1 = invokeFromNonScripted()

      d.appendBlockE(invokeFromScripted1) should produce(
        s"Transaction sent from smart account. Requires $ScriptExtraFee extra fee. " +
          s"Transaction involves 2 scripted assets. Requires ${2 * ScriptExtraFee} extra fee. " +
          s"Fee for InvokeScriptTransaction (${invokeFromScripted1.fee} in WAVES) " +
          s"does not exceed minimal value of ${FeeConstants(TransactionType.InvokeScript) * FeeUnit + 3 * ScriptExtraFee} WAVES"
      )
      d.appendBlockE(invokeFromNonScripted1) should produce(
        s"Transaction involves 2 scripted assets. Requires ${2 * ScriptExtraFee} extra fee. " +
          s"Fee for InvokeScriptTransaction (${invokeFromScripted1.fee} in WAVES) " +
          s"does not exceed minimal value of ${FeeConstants(TransactionType.InvokeScript) * FeeUnit + 2 * ScriptExtraFee} WAVES"
      )

      d.appendBlock()
      d.appendBlock()
      d.blockchainUpdater.height shouldBe activationHeight

      val invokeFromScripted2    = invokeFromScripted()
      val invokeFromNonScripted2 = invokeFromNonScripted()

      d.appendBlock(invokeFromNonScripted2)
      d.appendBlockE(invokeFromScripted2) should produce(
        s"Transaction sent from smart account. Requires $ScriptExtraFee extra fee. " +
          s"Fee for InvokeScriptTransaction (${invokeFromScripted1.fee} in WAVES) " +
          s"does not exceed minimal value of ${FeeConstants(TransactionType.InvokeScript) * FeeUnit + ScriptExtraFee} WAVES"
      )
    }
  }
}
