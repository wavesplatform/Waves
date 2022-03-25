package com.wavesplatform.state.diffs.ci

import cats.kernel.Monoid
import com.wavesplatform.account.*
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.{ContractLimits, compiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{utils as _, *}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import org.scalatest.EitherValues

class InvokeScriptLimitsTest extends PropSpec with WithState with DBCacheSettings with EitherValues {

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
    ))

  private val fsWithV6 = fsWithV5.copy(
    preActivatedFeatures = fsWithV5.preActivatedFeatures.updated(BlockchainFeatures.RideV6.id, 0)
  )

  Seq(V5, V6).foreach { version =>
    property(s"Allow not more ${ContractLimits.MaxCallableActionsAmount(version)} non-data actions for V${version.id}") {
      val wavesTransferAmount = (ContractLimits.MaxCallableActionsAmount(version) - 8) / 2
      val (preparingTxs1, invoke1, masterAddress, serviceAddress1) =
        scenario(
          masterContract(_, _, wavesTransferAmount, version),
          serviceContract(version, wavesTransferAmount),
          version
        )

      assertDiffAndState(Seq(TestBlock.create(preparingTxs1)), TestBlock.create(Seq(invoke1), Block.ProtoBlockVersion), features(version)) {
        case (diff, bc) =>
          diff.scriptResults(invoke1.id()).error shouldBe None
          bc.accountData(masterAddress, "key") shouldBe Some(IntegerDataEntry("key", 1))
          bc.accountData(serviceAddress1, "bar") shouldBe Some(IntegerDataEntry("bar", 1))
      }

      val (preparingTxs2, invoke2, _, _) =
        scenario(
          masterContract(_, _, wavesTransferAmount, version, extraAction = true),
          serviceContract(version, wavesTransferAmount),
          version
        )

      assertDiffEi(Seq(TestBlock.create(preparingTxs2)), TestBlock.create(Seq(invoke2), Block.ProtoBlockVersion), features(version)) { ei =>
        ei should produceRejectOrFailedDiff("Actions count limit is exceeded")
      }
    }
  }

  private def scenario(masterDApp: (Address, Alias) => DApp, serviceDApp: DApp, version: StdLibVersion): (Seq[Transaction], InvokeScriptTransaction, Address, Address) = {
    val master = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)
    val service = TxHelpers.signer(2)

    val fee = TxHelpers.ciFee(1, 2)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress),
      TxHelpers.genesis(service.toAddress)
    )
    val alias = Alias.create("alias").explicitGet()
    val aliasTx = TxHelpers.createAlias(alias.name, service, fee)
    val setMasterScript = TxHelpers.setScript(master, ContractScript(version, masterDApp(service.toAddress, alias)).explicitGet(), fee)
    val setServiceScript = TxHelpers.setScript(service, ContractScript(version, serviceDApp).explicitGet(), fee)
    val preparingTxs = genesis :+ aliasTx :+ setMasterScript :+ setServiceScript

    val invoke = TxHelpers.invoke(master.toAddress, func = Some("foo"), invoker = invoker, payments = List(Payment(10L, Waves)), fee = fee)

    (preparingTxs, invoke, master.toAddress, service.toAddress)
  }

  private def features(version: StdLibVersion): FunctionalitySettings =
    version match {
      case V5 => fsWithV5
      case _ => fsWithV6
    }

  private def serviceContract(version: StdLibVersion, wavesTransferAmount: Int): DApp = {
    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION ${version.id} #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           | @Callable(i)
           | func bar(a: ByteVector, an: String) = {
           |   if i.caller.bytes == a && addressFromPublicKey(i.callerPublicKey).bytes == a
           |   then
           |     let n = Issue(an, an, 1, 0, false, unit, 0)
           |     ([
           |       IntegerEntry("bar", 1),
           |       ${"ScriptTransfer(Address(a), 1, unit), " * wavesTransferAmount}
           |       BinaryEntry("asset", n.calculateAssetId()),
           |       n,
           |       ScriptTransfer(Address(a), 1, n.calculateAssetId())], ${wavesTransferAmount + 4})
           |   else
           |     throw("Bad caller")
           | }
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, version)
  }

  def masterContract(otherAcc: Address,
                     alias: Alias,
                     wavesTransferAmount: Int,
                     version: StdLibVersion,
                     extraAction: Boolean = false): DApp = {
    val extraLeaseAction = if (extraAction) s"Lease(Address(base58'$otherAcc'), 15)," else ""

    val expr = {
      val script =
        s"""
           |{-# STDLIB_VERSION ${version.id} #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-#SCRIPT_TYPE ACCOUNT#-}
           |
           | @Callable(i)
           | func foo() = {
           |  let b1 = wavesBalance(this)
           |  let ob1 = wavesBalance(Address(base58'$otherAcc'))
           |  if b1 == b1 && ob1 == ob1
           |  then
           |    let r = invoke(Alias("${alias.name}"), "bar", [this.bytes, "aaaaaaaa"], [AttachedPayment(unit, ${wavesTransferAmount + 4})])
           |    let r1 = invoke(Alias("${alias.name}"), "bar", [this.bytes, "bbbbbbbb"], [AttachedPayment(unit, ${wavesTransferAmount + 4})])
           |    if r == r1
           |    then
           |     let data = getIntegerValue(Address(base58'$otherAcc'), "bar")
           |     let b2 = wavesBalance(this)
           |     let ob2 = wavesBalance(Address(base58'$otherAcc'))
           |     let ab = assetBalance(this, getBinaryValue(Address(base58'$otherAcc'), "asset"))
           |     if data == 1
           |     then
           |      if ob1.regular+8 == ob2.regular && b1.regular == b2.regular+8 && ab == 1
           |      then
           |       let l = Lease(Address(base58'$otherAcc'), 23)
           |       [
           |        IntegerEntry("key", 1),
           |        Lease(Address(base58'$otherAcc'), 13),
           |        Lease(Address(base58'$otherAcc'), 15),
           |        $extraLeaseAction
           |        l,
           |        LeaseCancel(l.calculateLeaseId())
           |       ]
           |      else
           |       throw("Balance check failed")
           |    else
           |     throw("Bad state")
           |   else
           |    throw("Bad returned value")
           |  else
           |   throw("Imposible")
           | }
           |""".stripMargin
      Parser.parseContract(script).get.value
    }

    compileContractFromExpr(expr, version)
  }

  private def compileContractFromExpr(expr: Expressions.DAPP, stdLibVersion: StdLibVersion): DApp = {
    val ctx = {
      utils.functionCosts(stdLibVersion)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(stdLibVersion, useNewPowPrecision = true).withEnvironment[Environment],
            CryptoContext.build(Global, stdLibVersion).withEnvironment[Environment],
            WavesContext.build(
              Global,
              DirectiveSet(stdLibVersion, Account, DAppType).explicitGet()
            )
          )
        )
    }

    compiler.ContractCompiler(ctx.compilerContext, expr, stdLibVersion).explicitGet()
  }
}
