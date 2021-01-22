package com.wavesplatform.state.diffs.ci
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BinaryDataEntry, IntegerDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncDAppCases
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with EitherValues {

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id           -> 0,
      BlockchainFeatures.SmartAssets.id             -> 0,
      BlockchainFeatures.Ride4DApps.id              -> 0,
      BlockchainFeatures.FeeSponsorship.id          -> 0,
      BlockchainFeatures.DataTransaction.id         -> 0,
      BlockchainFeatures.BlockV5.id                 -> 0,
      BlockchainFeatures.ContinuationTransaction.id -> 0
    )
  )

  property("flash loan") {
    val exchangeRate            = 5
    val exchangeRateDiffPercent = 20
    val loanFeePercent          = 5
    val tradeAmount             = 1000

    def borrowerScript(
        assetA: ByteStr,
        assetB: ByteStr,
        exchangerABAddress: Address,
        exchangerBAAddress: Address,
        loanerAddress: Address,
        beneficiary: Address
    ) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let assetA = base58'$assetA'
                      | let assetB = base58'$assetB'
                      | let loanFeePercent = $loanFeePercent
                      | let exchangerABAddress = Address(base58'$exchangerABAddress')
                      | let exchangerBAAddress = Address(base58'$exchangerBAAddress')
                      | let loanerAddress      = Address(base58'$loanerAddress')
                      | let beneficiaryAddress = Address(base58'$beneficiary')        # TODO should be available as first caller
                      |
                      | @Callable(i)
                      | func trade(amount: Int) = {
                      |   strict startBalanceB = this.assetBalance(assetB)
                      |   strict r1 = Invoke(exchangerABAddress, "exchangeAB", [], [AttachedPayment(assetA, amount)])
                      |   strict diffB = this.assetBalance(assetB) - startBalanceB
                      |
                      |   strict r2 = Invoke(exchangerBAAddress, "exchangeBA", [], [AttachedPayment(assetB, diffB)])
                      |
                      |   let debt = amount.fraction(100 + loanFeePercent, 100)
                      |   let profit = this.assetBalance(assetA) - debt
                      |   [
                      |     ScriptTransfer(beneficiaryAddress, profit, assetA),
                      |     ScriptTransfer(loanerAddress, debt, assetA)
                      |   ]
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def loanerAScript(assetA: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let assetA = base58'$assetA'
                      | let loanFeePercent = $loanFeePercent
                      |
                      | @Callable(i)
                      | func loan(amount: Int, callback: String, borrower: ByteVector) = {
                      |   strict startBalance = this.assetBalance(assetA)
                      |   strict r = Invoke(Address(borrower), callback, [amount], [AttachedPayment(assetA, amount)])
                      |
                      |   let balanceDiff = this.assetBalance(assetA) - startBalance
                      |   let profit      = amount.fraction(loanFeePercent, 100)
                      |
                      |   if (balanceDiff < profit)
                      |     then
                      |       throw("debt is not paid: diff=" + balanceDiff.toString() + ", expected=" + profit.toString())
                      |     else
                      |       []
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def exchangerABScript(assetA: ByteStr, assetB: ByteStr) = {
      val script =
        s"""
             | {-# STDLIB_VERSION 5     #-}
             | {-# SCRIPT_TYPE ACCOUNT  #-}
             | {-# CONTENT_TYPE DAPP    #-}
             |
             | let aToBRate = $exchangeRate
             | let assetA = base58'$assetA'
             | let assetB = base58'$assetB'
             |
             | @Callable(i)
             | func exchangeAB() = {
             |   if (i.payments[0].assetId == assetA)
             |     then
             |       [ ScriptTransfer(i.caller, i.payments[0].amount * aToBRate, assetB) ]
             |     else
             |       throw("unexpected token")
             | }
           """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def exchangerBAScript(assetA: ByteStr, assetB: ByteStr) = {
      val script =
        s"""
             | {-# STDLIB_VERSION 5     #-}
             | {-# SCRIPT_TYPE ACCOUNT  #-}
             | {-# CONTENT_TYPE DAPP    #-}
             |
             | let aToBRate = $exchangeRate.fraction(100 - $exchangeRateDiffPercent, 100)
             | let assetA = base58'$assetA'
             | let assetB = base58'$assetB'
             |
             | @Callable(i)
             | func exchangeBA() = {
             |   if (i.payments[0].assetId == assetB)
             |     then
             |       [ ScriptTransfer(i.caller, i.payments[0].amount / aToBRate, assetA) ]
             |     else
             |       throw("unexpected token")
             | }
           """.stripMargin
      Parser.parseContract(script).get.value
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    val scenario =
      for {
        borrower    <- accountGen
        loaner      <- accountGen
        exchangerAB <- accountGen
        exchangerBA <- accountGen
        beneficiary <- accountGen
        ts          <- timestampGen
        fee         <- ciFee(1)

        assetAIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            exchangerAB,
            "AssetA",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()
        assetBIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            exchangerBA,
            "AssetB",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 2
          )
          .explicitGet()

        gTx1 = GenesisTransaction.create(borrower.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(loaner.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(exchangerAB.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx4 = GenesisTransaction.create(exchangerBA.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx5 = GenesisTransaction.create(beneficiary.toAddress, ENOUGH_AMT, ts).explicitGet()

        assetA = assetAIssue.id.value()
        assetB = assetBIssue.id.value()

        borrowerScriptR    = borrowerScript(assetA, assetB, exchangerAB.toAddress, exchangerBA.toAddress, loaner.toAddress, beneficiary.toAddress)
        loanerScriptR      = loanerAScript(assetA)
        exchangerABScriptR = exchangerABScript(assetA, assetB)
        exchangerBAScriptR = exchangerBAScript(assetA, assetB)

        b  = SetScriptTransaction.selfSigned(1.toByte, borrower, borrowerScriptR, fee, ts + 5).explicitGet()
        l  = SetScriptTransaction.selfSigned(1.toByte, loaner, loanerScriptR, fee, ts + 5).explicitGet()
        ab = SetScriptTransaction.selfSigned(1.toByte, exchangerAB, exchangerABScriptR, fee, ts + 5).explicitGet()
        ba = SetScriptTransaction.selfSigned(1.toByte, exchangerBA, exchangerBAScriptR, fee, ts + 5).explicitGet()

        transfer1 = TransferTransaction
          .selfSigned(2.toByte, exchangerAB, loaner.toAddress, IssuedAsset(assetA), Int.MaxValue, Waves, fee, ByteStr.empty, ts + 5)
          .explicitGet()
        transfer2 = TransferTransaction
          .selfSigned(2.toByte, exchangerAB, exchangerBA.toAddress, IssuedAsset(assetA), Int.MaxValue, Waves, fee, ByteStr.empty, ts + 5)
          .explicitGet()
        transfer3 = TransferTransaction
          .selfSigned(2.toByte, exchangerBA, exchangerAB.toAddress, IssuedAsset(assetB), Int.MaxValue, Waves, fee, ByteStr.empty, ts + 5)
          .explicitGet()

        fc = Terms.FUNCTION_CALL(
          FunctionHeader.User("loan"),
          List(
            CONST_LONG(tradeAmount),
            CONST_STRING("trade").explicitGet(),
            CONST_BYTESTR(ByteStr(borrower.toAddress.bytes)).explicitGet()
          )
        )
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            beneficiary,
            loaner.toAddress,
            Some(fc),
            Nil,
            fee * 100,
            Waves,
            InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts + 10
          )
          .explicitGet()

      } yield (
        Seq(gTx1, gTx2, gTx3, gTx4, gTx5, assetAIssue, assetBIssue, transfer1, transfer2, transfer3, b, l, ab, ba),
        invokeTx,
        assetA
      )

    forAll(scenario) {
      case (genesisTxs, invokeTx, tradedAsset) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invokeTx.id.value()) shouldBe None
            val expectingAmount = tradeAmount * (100 * 100 / (100 - exchangeRateDiffPercent) - loanFeePercent - 100) / 100
            diff.portfolios(invokeTx.senderAddress).assets shouldBe Map(IssuedAsset(tradedAsset) -> expectingAmount)
        }
    }
  }

  property("swap") {
    val exchangeRate = 5
    val tradeAmount  = 1000

    def stakingScript(usdN: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let usdN = base58'$usdN'
                      |
                      | @Callable(i)
                      | func cancelStake(amount: Int) = {
                      |   if (this.getIntegerValue(i.caller.toString()) >= amount)
                      |     then
                      |       [ ScriptTransfer(i.caller, amount, usdN) ]
                      |     else
                      |       throw("too big amount")
                      |
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def exchangerScript(usdN: ByteStr, stakingAddress: Address) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let usdN = base58'$usdN'
                      | let exchangeRate = $exchangeRate
                      | let staker = Address(base58'$stakingAddress')
                      |
                      | @Callable(i)
                      | func exchangeWavesUsdN() = {
                      |   if (i.payments[0].assetId != unit)
                      |     then
                      |       throw("unexpected asset")
                      |     else {
                      |       strict r = Invoke(staker, "cancelStake", [i.payments[0].amount * exchangeRate], [])
                      |       [ ScriptTransfer(i.caller, i.payments[0].amount * exchangeRate, usdN) ]
                      |     }
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    val scenario =
      for {
        invoker   <- accountGen
        staker    <- accountGen
        exchanger <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)

        usdNIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            staker,
            "USDN",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        gTx1 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(staker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(exchanger.toAddress, ENOUGH_AMT, ts).explicitGet()

        usdN = usdNIssue.id.value()

        stakingScriptR = stakingScript(usdN)
        exchangerR     = exchangerScript(usdN, staker.toAddress)

        s1 = SetScriptTransaction.selfSigned(1.toByte, staker, stakingScriptR, fee, ts + 5).explicitGet()
        s2 = SetScriptTransaction.selfSigned(1.toByte, exchanger, exchangerR, fee, ts + 5).explicitGet()

        d = DataTransaction
          .selfSigned(TxVersion.V2, staker, Seq(IntegerDataEntry(exchanger.toAddress.toString, tradeAmount * exchangeRate)), fee, ts + 5)
          .explicitGet()

        fc = Terms.FUNCTION_CALL(
          FunctionHeader.User("exchangeWavesUsdN"),
          Nil
        )
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            invoker,
            exchanger.toAddress,
            Some(fc),
            List(Payment(tradeAmount, Waves)),
            fee * 100,
            Waves,
            InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts + 10
          )
          .explicitGet()

      } yield (
        Seq(gTx1, gTx2, gTx3, usdNIssue, s1, s2, d),
        invokeTx,
        usdN
      )

    forAll(scenario) {
      case (genesisTxs, invokeTx, usdN) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invokeTx.id.value()) shouldBe None
            diff.portfolios(invokeTx.sender.toAddress).assets shouldBe Map(IssuedAsset(usdN) -> tradeAmount * exchangeRate)
        }
    }
  }

  property("add liquidity") {
    val stakeAmount = 1000
    val leaseAmount = 777

    def stakingScript(usdN: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let usdN = base58'$usdN'
                      |
                      | @Callable(i)
                      | func stake() = {
                      |   if (i.payments[0].assetId != usdN)
                      |     then
                      |       throw("unexpected asset")
                      |     else {
                      |       let currentLiquidity = this.getInteger(i.caller.toString()).valueOrElse(0)
                      |       [ IntegerEntry(i.caller.toString(), currentLiquidity + i.payments[0].amount) ]
                      |     }
                      |
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def exchangerScript(usdN: ByteStr, stakingAddress: Address, leasePool: Address, shareToken: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let usdN = base58'$usdN'
                      | let staker = Address(base58'$stakingAddress')
                      | let leasePool = Address(base58'$leasePool')
                      | let shareToken = base58'$shareToken'
                      |
                      | @Callable(i)
                      | func exchangeWavesUsdN() = {
                      |   if (i.payments[0].assetId != unit && i.payments[1].assetId != usdN)
                      |     then
                      |       throw("unexpected assets")
                      |     else {
                      |       strict r = Invoke(staker, "stake", [], [i.payments[1]])
                      |
                      |       let amountKey = "AMOUNT_" + i.caller.toString()
                      |       let idKey     = "ID_" + i.caller.toString()
                      |
                      |       let leasedAmount = this.getInteger(amountKey).valueOrElse(0)
                      |       let leaseId      = this.getString(idKey)
                      |
                      |       let newLease   = Lease(leasePool, leasedAmount + i.payments[0].amount)
                      |       let newLeaseId = calculateLeaseId(newLease)
                      |
                      |       let cancel = match leaseId {
                      |         case id: String => [ LeaseCancel(fromBase58String(id)) ]
                      |         case _          => [ StringEntry(idKey, toBase58String(newLeaseId)) ]
                      |       }
                      |
                      |       cancel ++
                      |       [
                      |         newLease,
                      |         ScriptTransfer(i.caller, 1, shareToken),
                      |         IntegerEntry(amountKey, leasedAmount + i.payments[0].amount)
                      |       ]
                      |     }
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    val scenario =
      for {
        invoker   <- accountGen
        staker    <- accountGen
        exchanger <- accountGen
        leasePool <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)

        usdNIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            staker,
            "USDN",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        shareTokenIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            exchanger,
            "Share",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        gTx1 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(staker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(exchanger.toAddress, ENOUGH_AMT, ts).explicitGet()

        usdN       = usdNIssue.id.value()
        shareToken = shareTokenIssue.id.value()

        stakingScriptR = stakingScript(usdN)
        exchangerR     = exchangerScript(usdN, staker.toAddress, leasePool.toAddress, shareToken)

        s1 = SetScriptTransaction.selfSigned(1.toByte, staker, stakingScriptR, fee, ts + 5).explicitGet()
        s2 = SetScriptTransaction.selfSigned(1.toByte, exchanger, exchangerR, fee, ts + 5).explicitGet()

        transfer1 = TransferTransaction
          .selfSigned(2.toByte, staker, invoker.toAddress, IssuedAsset(usdN), Int.MaxValue, Waves, fee, ByteStr.empty, ts + 5)
          .explicitGet()

        fc = Terms.FUNCTION_CALL(
          FunctionHeader.User("exchangeWavesUsdN"),
          Nil
        )
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            invoker,
            exchanger.toAddress,
            Some(fc),
            List(Payment(leaseAmount, Waves), Payment(stakeAmount, IssuedAsset(usdN))),
            fee * 100,
            Waves,
            InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts + 10
          )
          .explicitGet()

      } yield (
        Seq(gTx1, gTx2, gTx3, usdNIssue, shareTokenIssue, s1, s2, transfer1),
        invokeTx,
        usdN,
        shareToken
      )

    forAll(scenario) {
      case (genesisTxs, invokeTx, usdN, shareToken) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invokeTx.id.value()) shouldBe None
            diff.portfolios(invokeTx.sender.toAddress).assets shouldBe Map(
              IssuedAsset(usdN)       -> -stakeAmount,
              IssuedAsset(shareToken) -> 1
            )
        }
    }
  }

  property("decrease liquidity") {
    val stakeAmount = 1000
    val leaseAmount = 777

    def stakingScript(usdN: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let usdN = base58'$usdN'
                      |
                      | @Callable(i)
                      | func cancelStake(amount: Int) = {
                      |   if (this.getIntegerValue(i.caller.toString()) >= amount)
                      |     then
                      |       [ ScriptTransfer(i.caller, amount, usdN) ]
                      |     else
                      |       throw("too big amount")
                      |
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def exchangerScript(usdN: ByteStr, stakingAddress: Address, shareToken: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let usdN = base58'$usdN'
                      | let staker = Address(base58'$stakingAddress')
                      | let shareToken = base58'$shareToken'
                      |
                      | @Callable(i)
                      | func exchangeWavesUsdN() = {
                      |   if (i.payments[0].assetId != shareToken)
                      |     then
                      |       throw("unexpected asset")
                      |     else {
                      |       let stakeAmountKey = "STAKE_AMOUNT_" + i.caller.toString()
                      |       let leaseAmountKey = "LEASE_AMOUNT_" + i.caller.toString()
                      |       let leaseIdKey     = "LEASE_ID_" + i.caller.toString()
                      |
                      |       let stakedAmount = this.getInteger(stakeAmountKey).valueOrElse(0)
                      |       strict r = Invoke(staker, "cancelStake", [stakedAmount], [])
                      |
                      |       let leaseId = this.getBinaryValue(leaseIdKey)
                      |       let leaseAmount = this.getIntegerValue(leaseAmountKey)
                      |       [
                      |         LeaseCancel(leaseId),
                      |         ScriptTransfer(i.caller, stakedAmount, usdN),
                      |         ScriptTransfer(i.caller, leaseAmount, unit),
                      |         DeleteEntry(leaseAmountKey),
                      |         DeleteEntry(stakeAmountKey),
                      |         DeleteEntry(leaseIdKey)
                      |       ]
                      |     }
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    val scenario =
      for {
        invoker   <- accountGen
        staker    <- accountGen
        exchanger <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)

        usdNIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            staker,
            "USDN",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        shareTokenIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            invoker,
            "Share",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        gTx1 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(staker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(exchanger.toAddress, ENOUGH_AMT, ts).explicitGet()

        usdN       = usdNIssue.id.value()
        shareToken = shareTokenIssue.id.value()

        stakingScriptR = stakingScript(usdN)
        exchangerR     = exchangerScript(usdN, staker.toAddress, shareToken)

        s1 = SetScriptTransaction.selfSigned(1.toByte, staker, stakingScriptR, fee, ts + 5).explicitGet()
        s2 = SetScriptTransaction.selfSigned(1.toByte, exchanger, exchangerR, fee, ts + 5).explicitGet()

        transfer1 = TransferTransaction
          .selfSigned(2.toByte, staker, invoker.toAddress, IssuedAsset(usdN), Int.MaxValue, Waves, fee, ByteStr.empty, ts + 5)
          .explicitGet()

        lease = LeaseTransaction.selfSigned(2.toByte, exchanger, staker.toAddress, leaseAmount, fee, ts + 5).explicitGet()

        data = DataTransaction
          .selfSigned(
            2.toByte,
            exchanger,
            Seq(
              IntegerDataEntry(s"STAKE_AMOUNT_${invoker.toAddress}", stakeAmount),
              IntegerDataEntry(s"LEASE_AMOUNT_${invoker.toAddress}", leaseAmount),
              BinaryDataEntry(s"LEASE_ID_${invoker.toAddress}", lease.id.value())
            ),
            fee,
            ts + 5
          )
          .explicitGet()
        data2 = DataTransaction
          .selfSigned(
            2.toByte,
            staker,
            Seq(
              IntegerDataEntry(exchanger.toAddress.toString, stakeAmount)
            ),
            fee,
            ts + 5
          )
          .explicitGet()

        fc = Terms.FUNCTION_CALL(
          FunctionHeader.User("exchangeWavesUsdN"),
          Nil
        )
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            invoker,
            exchanger.toAddress,
            Some(fc),
            List(Payment(1, IssuedAsset(shareToken))),
            fee * 100,
            Waves,
            InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts + 10
          )
          .explicitGet()

      } yield (
        Seq(gTx1, gTx2, gTx3, usdNIssue, shareTokenIssue, s1, s2, transfer1, lease, data, data2),
        invokeTx,
        usdN,
        shareToken
      )

    forAll(scenario) {
      case (genesisTxs, invokeTx, usdN, shareToken) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invokeTx.id.value()) shouldBe None
            diff.portfolios(invokeTx.sender.toAddress).assets shouldBe Map(
              IssuedAsset(usdN)       -> stakeAmount,
              IssuedAsset(shareToken) -> -1
            )
        }
    }
  }

  property("early-bird") {
    val rewardAmount = 1000

    def swopMiningScript(swopToken: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let swopToken = base58'$swopToken'
                      |
                      | @Callable(i)
                      | func increase(address: ByteVector) = {
                      |   if (i.payments[0].assetId != swopToken)
                      |     then
                      |       throw("unexpected asset")
                      |     else {
                      |       let key = Address(address).toString()
                      |       let currentValue = this.getInteger(key).valueOrElse(0)
                      |       let newValue = currentValue + i.payments[0].amount
                      |       [ IntegerEntry(key, newValue) ]
                      |     }
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def earlyBirdScript(swopToken: ByteStr, swopMiner: Address) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let swopToken = base58'$swopToken'
                      | let swopMiner = Address(base58'$swopMiner')
                      |
                      | @Callable(i)
                      | func process() = {
                      |    let key = i.caller.toString()
                      |    let reward = this.getIntegerValue(key)
                      |    strict r = Invoke(swopMiner, "increase", [i.caller.bytes], [AttachedPayment(swopToken, reward)])
                      |    []
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    val scenario =
      for {
        invoker   <- accountGen
        swopMiner <- accountGen
        earlyBird <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)

        swopTokenIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            earlyBird,
            "Share",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        gTx1 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(swopMiner.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(earlyBird.toAddress, ENOUGH_AMT, ts).explicitGet()

        swopToken = swopTokenIssue.id.value()

        swopMiningScriptR = swopMiningScript(swopToken)
        exchangerR        = earlyBirdScript(swopToken, swopMiner.toAddress)

        s1 = SetScriptTransaction.selfSigned(1.toByte, swopMiner, swopMiningScriptR, fee, ts + 5).explicitGet()
        s2 = SetScriptTransaction.selfSigned(1.toByte, earlyBird, exchangerR, fee, ts + 5).explicitGet()

        data2 = DataTransaction
          .selfSigned(
            2.toByte,
            earlyBird,
            Seq(
              IntegerDataEntry(invoker.toAddress.toString, rewardAmount)
            ),
            fee,
            ts + 5
          )
          .explicitGet()

        fc = Terms.FUNCTION_CALL(
          FunctionHeader.User("process"),
          Nil
        )
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            invoker,
            earlyBird.toAddress,
            Some(fc),
            Nil,
            fee * 100,
            Waves,
            InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts + 10
          )
          .explicitGet()

      } yield (
        Seq(gTx1, gTx2, gTx3, swopTokenIssue, s1, s2, data2),
        invokeTx,
        swopToken
      )

    forAll(scenario) {
      case (genesisTxs, invokeTx, swopToken) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invokeTx.id.value()) shouldBe None
            diff.portfolios(invokeTx.dAppAddressOrAlias.asInstanceOf[Address]).assets shouldBe Map(IssuedAsset(swopToken) -> -rewardAmount)
        }
    }
  }

  property("receive all rewards") {
    val shareAmount = 777
    val swapAmount = 1000

    def swopMiningScript(swopToken: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let swopToken = base58'$swopToken'
                      |
                      | @Callable(i)
                      | func receive(address: ByteVector) = {
                      |    let key = Address(address).toString()
                      |    let amount = this.getInteger(key).valueOrElse(0)
                      |    [ ScriptTransfer(Address(address), amount, swopToken) ]
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    def earlyBirdScript(swopToken: ByteStr, swopMiner: Address, shareToken: ByteStr) = {
      val script = s"""
                      | {-# STDLIB_VERSION 5     #-}
                      | {-# SCRIPT_TYPE ACCOUNT  #-}
                      | {-# CONTENT_TYPE DAPP    #-}
                      |
                      | let swopToken = base58'$swopToken'
                      | let shareToken = base58'$shareToken'
                      | let swopMiner = Address(base58'$swopMiner')
                      |
                      | @Callable(i)
                      | func process() = {
                      |    strict r = Invoke(swopMiner, "receive", [i.caller.bytes], [])
                      |    [ ScriptTransfer(i.caller, $shareAmount, shareToken) ]
                      | }
                    """.stripMargin
      Some(ContractScript(V5, compileContractFromExpr(Parser.parseContract(script).get.value, V5)).explicitGet())
    }

    val scenario =
      for {
        invoker   <- accountGen
        swopMiner <- accountGen
        earlyBird <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)

        swopTokenIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            swopMiner,
            "Swop",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        shareTokenIssue = IssueTransaction
          .selfSigned(
            2.toByte,
            earlyBird,
            "Share",
            "",
            ENOUGH_AMT,
            8,
            reissuable = true,
            None,
            fee,
            ts + 1
          )
          .explicitGet()

        gTx1 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(swopMiner.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(earlyBird.toAddress, ENOUGH_AMT, ts).explicitGet()

        swopToken  = swopTokenIssue.id.value()
        shareToken = shareTokenIssue.id.value()

        swopMiningScriptR = swopMiningScript(swopToken)
        exchangerR        = earlyBirdScript(swopToken, swopMiner.toAddress, shareToken)

        s1 = SetScriptTransaction.selfSigned(1.toByte, swopMiner, swopMiningScriptR, fee, ts + 5).explicitGet()
        s2 = SetScriptTransaction.selfSigned(1.toByte, earlyBird, exchangerR, fee, ts + 5).explicitGet()

        data2 = DataTransaction
          .selfSigned(
            2.toByte,
            swopMiner,
            Seq(
              IntegerDataEntry(invoker.toAddress.toString, swapAmount)
            ),
            fee,
            ts + 5
          )
          .explicitGet()

        fc = Terms.FUNCTION_CALL(
          FunctionHeader.User("process"),
          Nil
        )
        invokeTx = InvokeScriptTransaction
          .selfSigned(
            TxVersion.V3,
            invoker,
            earlyBird.toAddress,
            Some(fc),
            Nil,
            fee * 100,
            Waves,
            InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts + 10
          )
          .explicitGet()

      } yield (
        Seq(gTx1, gTx2, gTx3, swopTokenIssue, shareTokenIssue, s1, s2, data2),
        invokeTx,
        shareToken
      )

    forAll(scenario) {
      case (genesisTxs, invokeTx, shareToken) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, _) =>
            diff.errorMessage(invokeTx.id.value()) shouldBe None
            diff.portfolios(invokeTx.dAppAddressOrAlias.asInstanceOf[Address]).assets shouldBe Map(IssuedAsset(shareToken) -> -777)
        }
    }
  }
}
