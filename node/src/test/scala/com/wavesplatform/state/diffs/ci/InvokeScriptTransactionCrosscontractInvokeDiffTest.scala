package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.{NoShrink, TransactionGen}
import com.wavesplatform.db.{DBCacheSettings, WithState}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.{EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.TestFunctionalitySettings
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class InvokeScriptTransactionCrosscontractCallDiffTest
    extends PropSpec
    with PropertyChecks
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

  property("Crosscontract call - internal invoke state update") {

    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    def contractMain(otherAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |    let nextDAppAddr = Address(base58'$otherAcc')
             |
             |    strict invEntry3BeforeIsDefined = isDefined(getString(nextDAppAddr, "$invokeEntry3Key"))
             |
             |    strict invResult = Invoke(nextDAppAddr, "bar", [], [])
             |
             |    let invEntry1ValIsOK = getIntegerValue(nextDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
             |    let invEntry2IsNotString = isDefined(getString(nextDAppAddr, "$invokeEntry2Key")) == false
             |    let invEntry2ValIsOK = getIntegerValue(nextDAppAddr, "$invokeEntry2Key") == $invokeEntry2NewVal
             |    let invEntry3AfterIsDeleted = isDefined(getString(nextDAppAddr, "$invokeEntry3Key")) == false
             |
             |
             |    if invEntry1ValIsOK && invEntry2IsNotString && invEntry2ValIsOK && invEntry3BeforeIsDefined && invEntry3AfterIsDeleted
             |    then
             |      ([], unit)
             |    else
             |      throw("Internal invoke state update error")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contractSecond(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar() = {
             |    (
             |      [IntegerEntry("$invokeEntry1Key", $invokeEntry1Val),
             |       IntegerEntry("$invokeEntry2Key", $invokeEntry2NewVal),
             |       DeleteEntry("$invokeEntry3Key")
             |      ],
             |      unit
             |    )
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }
    val scenario =
      for {
        mainAcc   <- accountGen
        invoker   <- accountGen
        secondAcc <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)
        gTx1 = GenesisTransaction.create(mainAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(secondAcc.toAddress, ENOUGH_AMT, ts).explicitGet()

        scriptMain   = ContractScript(V5, contractMain(secondAcc.toAddress))
        scriptSecond = ContractScript(V5, contractSecond())
        ssTxMain     = SetScriptTransaction.selfSigned(1.toByte, mainAcc, scriptMain.toOption, fee, ts + 5).explicitGet()
        ssTxSecond   = SetScriptTransaction.selfSigned(1.toByte, secondAcc, scriptSecond.toOption, fee, ts + 5).explicitGet()

        dataEntry    = StringDataEntry(invokeEntry2Key, "strData")
        dataTxSecond = DataTransaction.selfSigned(1.toByte, secondAcc, Seq(dataEntry), fee, ts + 6).explicitGet()

        dataEntry2    = StringDataEntry(invokeEntry3Key, "deleted entry")
        dataTxSecond2 = DataTransaction.selfSigned(1.toByte, secondAcc, Seq(dataEntry2), fee, ts + 6).explicitGet()

        fc       = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty)
        payments = List(Payment(10, Waves))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V3, invoker, mainAcc.toAddress, Some(fc), payments, fee, Waves, ts + 10)
          .explicitGet()
      } yield (Seq(gTx1, gTx2, gTx3, ssTxMain, ssTxSecond, dataTxSecond, dataTxSecond2), invokeTx, secondAcc.toAddress)

    forAll(scenario) {
      case (genesisTxs, invokeTx, secondDApp) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, bc) =>
            bc.accountData(secondDApp, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
            bc.accountData(secondDApp, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
        }
    }
  }

  property("Crosscontract call - same contract internal invoke - state update") {

    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    def contractMain(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |
             |    strict invEntry3BeforeIsDefined = isDefined(getString(this, "$invokeEntry3Key"))
             |
             |    strict invResult = Invoke(this, "bar", [], [])
             |
             |    let invEntry1ValIsOK = getIntegerValue(this, "$invokeEntry1Key") == $invokeEntry1Val
             |    let invEntry2IsNotString = isDefined(getString(this, "$invokeEntry2Key")) == false
             |    let invEntry2ValIsOK = getIntegerValue(this, "$invokeEntry2Key") == $invokeEntry2NewVal
             |    let invEntry3AfterIsDeleted = isDefined(getString(this, "$invokeEntry3Key")) == false
             |
             |
             |    if invEntry1ValIsOK && invEntry2IsNotString && invEntry2ValIsOK && invEntry3BeforeIsDefined && invEntry3AfterIsDeleted
             |    then
             |      ([], unit)
             |    else
             |      throw("Internal invoke state update error")
             | }
             |
             | @Callable(i)
             | func bar() = {
             |   (
             |      [IntegerEntry("$invokeEntry1Key", $invokeEntry1Val),
             |       IntegerEntry("$invokeEntry2Key", $invokeEntry2NewVal),
             |       DeleteEntry("$invokeEntry3Key")
             |      ],
             |      unit
             |   )
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val scenario =
      for {
        mainAcc <- accountGen
        invoker <- accountGen
        ts      <- timestampGen
        fee     <- ciFee(1)
        gTx1 = GenesisTransaction.create(mainAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()

        scriptMain = ContractScript(V5, contractMain())
        ssTxMain   = SetScriptTransaction.selfSigned(1.toByte, mainAcc, scriptMain.toOption, fee, ts + 5).explicitGet()

        dataEntry  = StringDataEntry(invokeEntry2Key, "strData")
        dataTxMain = DataTransaction.selfSigned(1.toByte, mainAcc, Seq(dataEntry), fee, ts + 6).explicitGet()

        dataEntry2  = StringDataEntry(invokeEntry3Key, "deleted entry")
        dataTxMain2 = DataTransaction.selfSigned(1.toByte, mainAcc, Seq(dataEntry2), fee, ts + 6).explicitGet()

        fc       = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty)
        payments = List(Payment(10, Waves))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V3, invoker, mainAcc.toAddress, Some(fc), payments, fee, Waves, ts + 10)
          .explicitGet()
      } yield (Seq(gTx1, gTx2, ssTxMain, dataTxMain, dataTxMain2), invokeTx, mainAcc.toAddress)

    forAll(scenario) {
      case (genesisTxs, invokeTx, mainDApp) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, bc) =>
            bc.accountData(mainDApp, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
            bc.accountData(mainDApp, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
        }
    }
  }

  property("Crosscontract call - multiple internal invokes - state update") {

    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    def contractMain(secondAcc: Address, thirdAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func foo() = {
             |    let secondDAppAddr = Address(base58'$secondAcc')
             |    let thirdDAppAddr = Address(base58'$thirdAcc')
             |
             |    strict invResult = Invoke(secondDAppAddr, "bar", [], [])
             |
             |    let thirdDAppDataEntryIsOK = getIntegerValue(thirdDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
             |
             |    if thirdDAppDataEntryIsOK
             |    then
             |      ([], unit)
             |    else
             |      throw("Internal invoke chain state update error")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contractSecond(thirdAcc: Address): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func bar() = {
             |    let thirdDAppAddr = Address(base58'$thirdAcc')
             |
             |    strict invResult = Invoke(thirdDAppAddr, "bar", [], [])
             |
             |    let thirdDAppDataEntryIsOK = getIntegerValue(thirdDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
             |
             |    if thirdDAppDataEntryIsOK
             |    then
             |      сделать экшон с переводом ассета
             |      ([], unit)
             |    else
             |      throw("Internal invoke chain state update error")
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    def contractThird(): DApp = {
      val expr = {
        val script =
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |{-#SCRIPT_TYPE ACCOUNT#-}
             |
             | @Callable(i)
             | func baz() = {
             |    (
             |      [
             |        IntegerEntry("$invokeEntry1Key", $invokeEntry1Val)
             |      ],
             |      unit
             |    )
             | }
             |""".stripMargin
        Parser.parseContract(script).get.value
      }

      compileContractFromExpr(expr, V5)
    }

    val scenario =
      for {
        mainAcc   <- accountGen
        invoker   <- accountGen
        secondAcc <- accountGen
        thirdAcc <- accountGen
        ts        <- timestampGen
        fee       <- ciFee(1)
        gTx1 = GenesisTransaction.create(mainAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx2 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx3 = GenesisTransaction.create(secondAcc.toAddress, ENOUGH_AMT, ts).explicitGet()
        gTx4 = GenesisTransaction.create(thirdAcc.toAddress, ENOUGH_AMT, ts).explicitGet()

        scriptMain   = ContractScript(V5, contractMain(secondAcc.toAddress, thirdAcc.toAddress))
        scriptSecond = ContractScript(V5, contractSecond(thirdAcc.toAddress))
        scriptThird = ContractScript(V5, contractThird())
        ssTxMain     = SetScriptTransaction.selfSigned(1.toByte, mainAcc, scriptMain.toOption, fee, ts + 5).explicitGet()
        ssTxSecond   = SetScriptTransaction.selfSigned(1.toByte, secondAcc, scriptSecond.toOption, fee, ts + 5).explicitGet()
        ssTxThird   = SetScriptTransaction.selfSigned(1.toByte, thirdAcc, scriptThird.toOption, fee, ts + 5).explicitGet()

        fc       = Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty)
        payments = List(Payment(10, Waves))
        invokeTx = InvokeScriptTransaction
          .selfSigned(TxVersion.V3, invoker, mainAcc.toAddress, Some(fc), payments, fee, Waves, ts + 10)
          .explicitGet()
      } yield (Seq(gTx1, gTx2, gTx3, gTx4, ssTxMain, ssTxSecond, ssTxThird), invokeTx, secondAcc.toAddress)

    forAll(scenario) {
      case (genesisTxs, invokeTx, secondDApp) =>
        assertDiffAndState(Seq(TestBlock.create(genesisTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
          case (diff, bc) =>
            bc.accountData(secondDApp, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
            bc.accountData(secondDApp, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
        }
    }
  }
}
