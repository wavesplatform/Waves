package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.{IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import org.scalatest.EitherValues

class SyncInvokeDiffTest extends PropSpec with WithDomain with DBCacheSettings with EitherValues {
  import DomainPresets._

  private val fsWithV5 = RideV5.blockchainSettings.functionalitySettings

  property("Crosscontract call - internal invoke state update") {
    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    def contractMain(otherAcc: Address): Script = TestCompiler(V5).compileContract(
      s"""
      | @Callable(i)
      | func foo() = {
      |    let nextDAppAddr = Address(base58'$otherAcc')
      |
      |    strict invEntry3BeforeIsDefined = isDefined(getString(nextDAppAddr, "$invokeEntry3Key"))
      |
      |    strict invResult = invoke(nextDAppAddr, "bar", [], [])
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
    )

    def contractSecond(): Script = TestCompiler(V5).compileContract(s"""
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
      |""".stripMargin)

    val invoker   = TxHelpers.signer(0)
    val mainAcc   = TxHelpers.signer(1)
    val secondAcc = TxHelpers.signer(2)

    val gTx1 = TxHelpers.genesis(mainAcc.toAddress)
    val gTx2 = TxHelpers.genesis(invoker.toAddress)
    val gTx3 = TxHelpers.genesis(secondAcc.toAddress)

    val ssTxMain   = TxHelpers.setScript(mainAcc, contractMain(secondAcc.toAddress))
    val ssTxSecond = TxHelpers.setScript(secondAcc, contractSecond())

    val dataEntry    = StringDataEntry(invokeEntry2Key, "strData")
    val dataTxSecond = TxHelpers.data(secondAcc, Seq(dataEntry))

    val dataEntry2    = StringDataEntry(invokeEntry3Key, "deleted entry")
    val dataTxSecond2 = TxHelpers.data(secondAcc, Seq(dataEntry2))

    val payments     = List(Payment(10L, Waves))
    val invokeTx     = TxHelpers.invoke(mainAcc.toAddress, Some("foo"), payments = payments)
    val preparingTxs = Seq(gTx1, gTx2, gTx3, ssTxMain, ssTxSecond, dataTxSecond, dataTxSecond2)

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.errorMessage(invokeTx.id()) shouldBe None
        bc.accountData(secondAcc.toAddress, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
        bc.accountData(secondAcc.toAddress, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
    }
  }

  property("Crosscontract call - same contract internal invoke - state update") {
    val (invokeEntry1Key, invokeEntry1Val)    = ("entry1", 42)
    val (invokeEntry2Key, invokeEntry2NewVal) = ("entry2", 100500)
    val invokeEntry3Key                       = "entry3"

    val contractMain = TestCompiler(V5).compileContract(s"""
      | @Callable(i)
      | func foo() = {
      |    strict invEntry3BeforeIsDefined = isDefined(getString(this, "$invokeEntry3Key"))
      |
      |    strict invResult = invoke(this, "bar", [], [])
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
      |""".stripMargin)

    val invoker = TxHelpers.signer(0)
    val mainAcc = TxHelpers.signer(1)
    val gTx1    = TxHelpers.genesis(mainAcc.toAddress)
    val gTx2    = TxHelpers.genesis(invoker.toAddress)

    val ssTxMain = TxHelpers.setScript(mainAcc, contractMain)

    val dataEntry  = StringDataEntry(invokeEntry2Key, "strData")
    val dataTxMain = TxHelpers.data(mainAcc, Seq(dataEntry))

    val dataEntry2  = StringDataEntry(invokeEntry3Key, "deleted entry")
    val dataTxMain2 = TxHelpers.data(mainAcc, Seq(dataEntry2))

    val payments     = List(Payment(10L, Waves))
    val invokeTx     = TxHelpers.invoke(mainAcc.toAddress, Some("foo"), payments = payments)
    val preparingTxs = Seq(gTx1, gTx2, ssTxMain, dataTxMain, dataTxMain2)

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.errorMessage(invokeTx.id()) shouldBe None
        bc.accountData(mainAcc.toAddress, invokeEntry1Key) shouldBe Some(IntegerDataEntry(invokeEntry1Key, invokeEntry1Val))
        bc.accountData(mainAcc.toAddress, invokeEntry2Key) shouldBe Some(IntegerDataEntry(invokeEntry2Key, invokeEntry2NewVal))
    }
  }

  property("Crosscontract call - multiple internal invokes - state update") {
    val (invokeEntry1Key, invokeEntry1Val) = ("entry1", 42)
    val transferAssetAmount                = 123
    val paymentAssetAmount                 = 456

    def paymentAssetScript(thirdAcc: Address) =
      TestCompiler(V5).compileExpression(
        s"""
           | getIntegerValue(Address(base58'$thirdAcc'), "$invokeEntry1Key") == $invokeEntry1Val
         """.stripMargin
      )

    def transferAssetScript(thirdAcc: Address) =
      TestCompiler(V5).compileExpression(
        s"""
           | getIntegerValue(Address(base58'$thirdAcc'), "$invokeEntry1Key") == $invokeEntry1Val
         """.stripMargin
      )

    def contractMain(secondAcc: Address, thirdAcc: Address, paymentAsset: ByteStr): Script = TestCompiler(V5).compileContract(s"""
      | @Callable(i)
      | func foo() = {
      |    let secondDAppAddr = Address(base58'$secondAcc')
      |    let thirdDAppAddr = Address(base58'$thirdAcc')
      |
      |    strict invBarResult = invoke(secondDAppAddr, "bar", [], [])
      |
      |    let thirdDAppDataEntryIsOK = getIntegerValue(thirdDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
      |
      |    strict invAnotherBazResult = invoke(
      |      thirdDAppAddr,
      |      "anotherBaz",
      |      [],
      |      [AttachedPayment(base58'$paymentAsset', $paymentAssetAmount)])
      |
      |    if thirdDAppDataEntryIsOK
      |    then
      |      ([], unit)
      |    else
      |      throw("Internal invoke chain state update error")
      | }
    """.stripMargin)

    def contractSecond(thirdAcc: Address, transferAsset: ByteStr): Script = TestCompiler(V5).compileContract(s"""
      | @Callable(i)
      | func bar() = {
      |    let thirdDAppAddr = Address(base58'$thirdAcc')
      |
      |    strict invBazResult = invoke(thirdDAppAddr, "baz", [], [])
      |
      |    let thirdDAppDataEntryIsOK = getIntegerValue(thirdDAppAddr, "$invokeEntry1Key") == $invokeEntry1Val
      |
      |    if thirdDAppDataEntryIsOK
      |    then
      |      ([ScriptTransfer(thirdDAppAddr, $transferAssetAmount, base58'$transferAsset')], unit)
      |    else
      |      throw("Internal invoke chain state update error")
      | }
      |""".stripMargin)

    val contractThird = TestCompiler(V5).compileContract(s"""
      | @Callable(i)
      | func baz() = {
      |    (
      |      [
      |        IntegerEntry("$invokeEntry1Key", $invokeEntry1Val)
      |      ],
      |      unit
      |    )
      | }
      |
      | @Callable(i)
      | func anotherBaz() = {
      |    ([], unit)
      | }
      |""".stripMargin)

    val invoker   = TxHelpers.signer(0)
    val mainAcc   = TxHelpers.signer(1)
    val secondAcc = TxHelpers.signer(2)
    val thirdAcc  = TxHelpers.signer(3)

    val paymentIssue  = TxHelpers.issue(mainAcc, script = Some(paymentAssetScript(thirdAcc.toAddress)))
    val transferIssue = TxHelpers.issue(secondAcc, script = Some(transferAssetScript(thirdAcc.toAddress)))

    val gTx1 = TxHelpers.genesis(mainAcc.toAddress)
    val gTx2 = TxHelpers.genesis(invoker.toAddress)
    val gTx3 = TxHelpers.genesis(secondAcc.toAddress)
    val gTx4 = TxHelpers.genesis(thirdAcc.toAddress)

    val ssTxMain   = TxHelpers.setScript(mainAcc, contractMain(secondAcc.toAddress, thirdAcc.toAddress, paymentIssue.id()))
    val ssTxSecond = TxHelpers.setScript(secondAcc, contractSecond(thirdAcc.toAddress, transferIssue.id()))
    val ssTxThird  = TxHelpers.setScript(thirdAcc, contractThird)

    val payments     = List(Payment(10L, Waves))
    val invokeTx     = TxHelpers.invoke(mainAcc.toAddress, Some("foo"), payments = payments)
    val preparingTxs = Seq(gTx1, gTx2, gTx3, gTx4, ssTxMain, ssTxSecond, ssTxThird, paymentIssue, transferIssue)

    assertDiffAndState(Seq(TestBlock.create(preparingTxs)), TestBlock.create(Seq(invokeTx), Block.ProtoBlockVersion), fsWithV5) {
      case (diff, bc) =>
        diff.errorMessage(invokeTx.id()) shouldBe None
        bc.balance(thirdAcc.toAddress, IssuedAsset(transferIssue.id())) shouldBe transferAssetAmount
        bc.balance(thirdAcc.toAddress, IssuedAsset(paymentIssue.id())) shouldBe paymentAssetAmount
    }
  }
}
