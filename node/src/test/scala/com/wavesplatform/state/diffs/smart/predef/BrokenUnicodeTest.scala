package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BrokenUnicodeTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDomain
    with EitherValues {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val activationHeight = 4
  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> activationHeight
    ),
    estimatorPreCheckHeight = Int.MaxValue
  )

  private val u1 = "\ud87e"
  private val u2 = "\udc1a"
  private val u3 = "\ud83e"
  private val u4 = "\udd26"

  private val checkNoFixV1V2 =
    s"""
       | take("x冬x", 2)      == "x$u1"   &&
       | size("x冬x")         == 4        &&
       | drop("x冬x", 2)      == "${u2}x" &&
       | takeRight("x冬x", 2) == "${u2}x" &&
       | dropRight("x冬x", 2) == "x$u1"
     """.stripMargin

  private val checkFixV1V2 =
    s"""
       | take("x冬x", 2)      == "x冬" &&
       | size("x冬x")         == 3     &&
       | drop("x冬x", 2)      == "x"   &&
       | takeRight("x冬x", 2) == "冬x" &&
       | dropRight("x冬x", 2) == "x"
     """.stripMargin

  private val checkNoFixV3 =
    s"""
       | $checkNoFixV1V2 &&
       |
       | "x冬xqweqwe".indexOf("we")        == 5 &&
       | "x冬xqweqwe".indexOf("we", 5)     == 5 &&
       | "x冬xqweqwe".lastIndexOf("we")    == 8 &&
       | "x冬xqweqwe".lastIndexOf("we", 5) == 5 &&
       |
       | "aaa冬bbb".indexOf("$u1")        == 3 &&
       | "aaa冬bbb".indexOf("$u1", 2)     == 3 &&
       | "aaa冬bbb".lastIndexOf("$u1")    == 3 &&
       | "aaa冬bbb".lastIndexOf("$u1", 5) == 3 &&
       |
       |  "x冬x".split("").size() == 4                                                 &&
       |  "冬x🤦冬".split("")     == [ "$u1", "$u2", "x", "$u3", "$u4", "$u1", "$u2" ] &&
       |  "冬x🤦冬".split("$u1")  == [ "", "${u2}x🤦", "$u2"]
     """.stripMargin

  private val checkFixV3 =
    s"""
       | $checkFixV1V2 &&
       |
       | "x冬xqweqwe".indexOf("we")        == 4 &&
       | "x冬xqweqwe".indexOf("we", 5)     == 7 &&
       | "x冬xqweqwe".lastIndexOf("we")    == 7 &&
       | "x冬xqweqwe".lastIndexOf("we", 5) == 4 &&
       |
       | "aaa冬bbb".indexOf("$u1")        == unit &&
       | "aaa冬bbb".indexOf("$u1", 2)     == unit &&
       | "aaa冬bbb".lastIndexOf("$u1")    == unit &&
       | "aaa冬bbb".lastIndexOf("$u1", 5) == unit &&
       |
       | "x冬x".split("").size()   == 3                        &&
       | "冬x🤦冬".split("")       == [ "冬", "x", "🤦", "冬" ] &&
       | "冬x🤦冬".split("$u1")    == [ "冬x🤦冬" ]
     """.stripMargin

  private val checkNoFixV4 =
    s"""
       | $checkNoFixV3 &&
       | "x冬x".contains("x$u1")
     """.stripMargin

  private val checkFixV4AndNext =
    s"""
       | $checkFixV3 &&
       | !"x冬x".contains("x$u1")
     """.stripMargin

  private val checkNoFixV3DApp =
    s"""
       | @Callable(i)
       | func default() = {
       |   strict r = if ($checkNoFixV3) then true else throw("DApp V3 no fix error")
       |   WriteSet([])
       | }
     """.stripMargin

  private val checkFixV3DApp =
    s"""
       | @Callable(i)
       | func default() = {
       |   strict r = if ($checkFixV3) then true else throw("DApp V3 fix error")
       |   WriteSet([])
       | }
     """.stripMargin

  private val checkNoFixV4DApp =
    s"""
       | @Callable(i)
       | func default() = {
       |   strict r = if ($checkNoFixV4) then true else throw("DApp V4 no fix error")
       |   []
       | }
     """.stripMargin

  private val checkFixV4AndNextDApp =
    s"""
       | @Callable(i)
       | func default() = {
       |   strict r = if ($checkFixV4AndNext) then true else throw("DApp V4 fix error")
       |   []
       | }
     """.stripMargin

  private def checkNoFixScript(v: StdLibVersion) = TestCompiler(v).compileExpression(
    v match {
      case V1 | V2 => checkNoFixV1V2
      case V3      => checkNoFixV3
      case V4      => checkNoFixV4
      case _       => throw new RuntimeException(s"Unexpected $v")
    },
    allowIllFormedStrings = true
  )

  private def checkFixScript(v: StdLibVersion) = TestCompiler(v).compileExpression(
    v match {
      case V1 | V2 => checkFixV1V2
      case V3      => checkFixV3
      case _       => checkFixV4AndNext
    },
    allowIllFormedStrings = true
  )

  private def checkNoFixDAppScript(v: StdLibVersion) = TestCompiler(v).compileContract(
    v match {
      case V3 => checkNoFixV3DApp
      case V4 => checkNoFixV4DApp
      case _  => throw new RuntimeException(s"Unexpected $v")
    },
    allowIllFormedStrings = true
  )

  private def checkFixDAppScript(v: StdLibVersion) = TestCompiler(v).compileContract(
    v match {
      case V3 => checkFixV3DApp
      case _  => checkFixV4AndNextDApp
    },
    allowIllFormedStrings = true
  )

  private val allVersions              = DirectiveDictionary[StdLibVersion].all
  private val versionsBeforeActivation = allVersions.filter(_ < V5)

  private val allDAppVersions              = allVersions.filter(_ >= V3)
  private val dAppVersionsBeforeActivation = versionsBeforeActivation.filter(_ >= V3)

  private val scenario =
    for {
      recipient     <- accountGen
      invoker       <- accountGen
      accWithFix    <- Gen.listOfN(allVersions.size, accountGen).map(_.zip(allVersions))
      accWithNoFix  <- Gen.listOfN(versionsBeforeActivation.size, accountGen).map(_.zip(versionsBeforeActivation))
      dAppWithFix   <- Gen.listOfN(allDAppVersions.size, accountGen).map(_.zip(allDAppVersions))
      dAppWithNoFix <- Gen.listOfN(dAppVersionsBeforeActivation.size, accountGen).map(_.zip(dAppVersionsBeforeActivation))
      fee           <- ciFee()
      genesisTxs     = (accWithFix ::: accWithNoFix ::: dAppWithFix ::: dAppWithNoFix).map(a => GenesisTransaction.create(a._1.toAddress, ENOUGH_AMT, ts).explicitGet())
      invokerGenesis = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      setNoFix       = accWithNoFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkNoFixScript(a._2)), fee, ts).explicitGet())
      setFix         = accWithFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkFixScript(a._2)), fee, ts).explicitGet())
      setNoFixDApp   = dAppWithNoFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkNoFixDAppScript(a._2)), fee, ts).explicitGet())
      setFixDApp     = dAppWithFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkFixDAppScript(a._2)), fee, ts).explicitGet())
      checkFix = accWithFix
        .map(
          a =>
            TransferTransaction
              .selfSigned(TxVersion.V2, a._1, recipient.toAddress, Waves, 1, Waves, fee, ByteStr.empty, ts)
              .explicitGet()
        )
      checkNoFix = () =>
        accWithNoFix.map(
          a =>
            TransferTransaction
              .selfSigned(TxVersion.V2, a._1, recipient.toAddress, Waves, 1, Waves, fee, ByteStr.empty, ts)
              .explicitGet()
        )
      checkFixDApp = dAppWithFix
        .map(
          a =>
            InvokeScriptTransaction
              .selfSigned(TxVersion.V2, invoker, a._1.toAddress, None, Nil, fee, Waves, ts)
              .explicitGet()
        )
      checkNoFixDApp = () =>
        dAppWithNoFix.map(
          a =>
            InvokeScriptTransaction
              .selfSigned(TxVersion.V2, invoker, a._1.toAddress, None, Nil, fee, Waves, ts)
              .explicitGet()
        )
    } yield (invokerGenesis :: genesisTxs, setNoFix, setFix, checkFix, checkNoFix, setNoFixDApp, setFixDApp, checkFixDApp, checkNoFixDApp)

  property(s"string functions return correct results for unicode input after ${BlockchainFeatures.SynchronousCalls} activation") {
    val (genesisTxs, setNoFix, setFix, checkFix, checkNoFix, setNoFixDApp, setFixDApp, checkFixDApp, checkNoFixDApp) = scenario.sample.get
    withDomain(domainSettingsWithFS(fs)) { d =>
      d.appendBlock(genesisTxs: _*)

      val checkNoFix1     = checkNoFix()
      val checkNoFixDApp1 = checkNoFixDApp()
      d.appendBlock(setNoFix ::: setNoFixDApp: _*)

      val checkNoFixTxs = checkNoFix1 ::: checkNoFixDApp1
      d.appendBlock(checkNoFixTxs: _*)
      checkNoFixTxs.foreach(tx => d.blockchain.transactionMeta(tx.id.value()).get._2 shouldBe true)

      d.appendBlock()
      d.blockchain.height shouldBe activationHeight

      val checkFixTxs = checkFix ::: checkFixDApp
      d.appendBlock(setFix ::: setFixDApp: _*)
      d.appendBlock(checkFixTxs: _*)
      checkFixTxs.foreach(tx => d.blockchain.transactionMeta(tx.id.value()).get._2 shouldBe true)
      checkNoFix().foreach { tx =>
        (the[RuntimeException] thrownBy d.appendBlock(tx)).getMessage should include("TransactionNotAllowedByScript")
      }
      checkNoFixDApp().foreach { tx =>
        (the[RuntimeException] thrownBy d.appendBlock(tx)).getMessage should include("ScriptExecutionError(error = DApp")
      }
    }
  }
}
