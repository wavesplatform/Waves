package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalatest.{FreeSpec, Inside, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContinuationStateSpec extends FreeSpec with Matchers with WithDomain with TransactionGen with PropertyChecks with NoShrink with Inside {
  private val time          = new TestTime
  private def nextTs        = time.getTimestamp()

  private def genesisBlock(genesisTs: Long, initialBalances: Map[Address, Long]): Block = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](SignatureLength)(0)),
    initialBalances.map { case (address, initialBalance) => GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet() }.toSeq
  )

  private val settings = {
    val fs = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.SmartAccounts.id           -> 0,
        BlockchainFeatures.Ride4DApps.id              -> 0,
        BlockchainFeatures.BlockV5.id                 -> 0,
        BlockchainFeatures.ContinuationTransaction.id -> 0
      )
    )
    defaultDomainSettings.copy(blockchainSettings = defaultDomainSettings.blockchainSettings.copy(functionalitySettings = fs))
  }

  private def continuation(i: InvokeScriptTransaction, step: Int): ContinuationTransaction =
    ContinuationTransaction(i.id.value(), step, fee = 0L, Waves)

  private def compile(scriptText: String): Script =
    ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()._1

  private val dApp: Script =
    compile(
      s"""
         | {-# STDLIB_VERSION 5 #-}
         | {-# CONTENT_TYPE DAPP #-}
         |
         | @Callable(i)
         | func default() = {
         |   let a = !(${List.fill(40)("sigVerify(base64'', base64'', base64'')").mkString("||")})
         |   if (a)
         |     then
         |       [BooleanEntry("isAllowed", true)]
         |     else
         |       throw("unexpected")
         | }
           """.stripMargin
    )

  private val preconditions = for {
    caller  <- accountGen
    dAppAcc <- accountGen
    fee     <- smallFeeGen
    timestamp     = nextTs
    setScript     = SetScriptTransaction.selfSigned(TxVersion.V2, dAppAcc, Some(dApp), fee, timestamp + 1).explicitGet()
    paymentAmount = 1234567L
    makeInvoke = () => InvokeScriptTransaction
      .selfSigned(
        TxVersion.V3,
        caller,
        dAppAcc.toAddress,
        None,
        Seq(Payment(paymentAmount, Waves)),
        fee * 10,
        Waves,
        InvokeScriptTransaction.DefaultExtraFeePerStep,
        nextTs
      )
      .explicitGet()
  } yield (timestamp, caller.toAddress, dAppAcc.toAddress, setScript, makeInvoke, paymentAmount)

  "Continuations" - {
    "one after another for one dApp" in forAll(preconditions) {
      case (timestamp, caller, dAppAcc, setScript, makeInvoke, paymentAmount) =>
        withDomain(settings) { d =>
          d.appendBlock(genesisBlock(timestamp, Map(caller -> ENOUGH_AMT, dAppAcc -> ENOUGH_AMT)))
          d.appendBlock(setScript)

          def performAndAssertChain(invoke: InvokeScriptTransaction) = {
            val address = d.levelDBWriter.resolveAlias(invoke.dAppAddressOrAlias).explicitGet()
            val startCallerBalance = d.balance(caller)
            val startDAppBalance   = d.balance(dAppAcc)

            d.appendBlock(invoke)
            d.balance(caller) shouldBe startCallerBalance - invoke.fee - paymentAmount
            d.balance(dAppAcc) shouldBe startDAppBalance + paymentAmount
            inside(d.blockchainUpdater.continuationStates.toList) {
              case List((`address`, (0, ContinuationState.InProgress(_, _, _)))) =>
            }

            d.appendBlock(continuation(invoke, 0))
            d.balance(caller) shouldBe startCallerBalance - invoke.fee - paymentAmount
            d.balance(dAppAcc) shouldBe startDAppBalance + paymentAmount
            inside(d.blockchainUpdater.continuationStates.toList) {
              case List((`address`, (1, ContinuationState.InProgress(_, _, _)))) =>
            }

            d.appendBlock(continuation(invoke, 1))
            d.balance(caller) shouldBe startCallerBalance - 3 * FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit - paymentAmount
            d.balance(dAppAcc) shouldBe startDAppBalance + paymentAmount
            d.blockchainUpdater.continuationStates shouldBe Map((address, (2, ContinuationState.Finished)))
            d.blockchainUpdater.accountData(dAppAcc, "isAllowed") shouldBe Some(BooleanDataEntry("isAllowed", true))
          }

          performAndAssertChain(makeInvoke())
          performAndAssertChain(makeInvoke())
          performAndAssertChain(makeInvoke())
        }
    }

    "disallow unordered continuations" in forAll(preconditions) {
      case (timestamp, caller, dAppAcc, setScript, makeInvoke, _) =>
        withDomain(settings) { d =>
          d.appendBlock(genesisBlock(timestamp, Map(caller -> ENOUGH_AMT, dAppAcc -> ENOUGH_AMT)))
          d.appendBlock(setScript)

          val invoke = makeInvoke()

          d.appendBlock(invoke)
          d.appendBlock(continuation(invoke, 0))
          an[RuntimeException] should be thrownBy d.appendBlock(continuation(invoke, 0))
          an[RuntimeException] should be thrownBy d.appendBlock(continuation(invoke, 2))
          an[RuntimeException] should be thrownBy d.appendBlock(continuation(invoke, Int.MaxValue))
          an[RuntimeException] should be thrownBy d.appendBlock(continuation(invoke, -3))
          d.appendBlock(continuation(invoke, 1))

          val address = d.levelDBWriter.resolveAlias(invoke.dAppAddressOrAlias).explicitGet()
          d.blockchainUpdater.continuationStates shouldBe Map((address, (2, ContinuationState.Finished)))
          d.blockchainUpdater.accountData(dAppAcc, "isAllowed") shouldBe Some(BooleanDataEntry("isAllowed", true))

          an[RuntimeException] should be thrownBy d.appendBlock(continuation(invoke, 2))
        }
    }
  }
}
