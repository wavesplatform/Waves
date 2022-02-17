package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.EitherValues

class BrokenUnicodeTest extends PropSpec with WithDomain with EitherValues {

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

  private val scenario = {
    val recipient = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)
    val accWithFix = (1 to allVersions.size).map(idx => TxHelpers.signer(idx + 1)).zip(allVersions).toList
    val accWithNoFix = (1 to versionsBeforeActivation.size).map(idx => TxHelpers.signer(idx + allVersions.size + 1)).zip(versionsBeforeActivation).toList
    val dAppWithFix = (1 to allDAppVersions.size).map(idx => TxHelpers.signer(idx + allVersions.size + versionsBeforeActivation.size + 1)).zip(allDAppVersions).toList
    val dAppWithNoFix = (1 to dAppVersionsBeforeActivation.size).map(idx => TxHelpers.signer(idx + allVersions.size + versionsBeforeActivation.size + allDAppVersions.size + 1)).zip(dAppVersionsBeforeActivation).toList

    val balances = (accWithFix ::: accWithNoFix ::: dAppWithFix ::: dAppWithNoFix)
      .map { case (acc, _) => AddrWithBalance(acc.toAddress) }
    val invokerBalance = AddrWithBalance(invoker.toAddress)

    val setNoFix = accWithNoFix.map { case (acc, v) => TxHelpers.setScript(acc, checkNoFixScript(v)) }
    val setFix = accWithFix.map { case (acc, v) => TxHelpers.setScript(acc, checkFixScript(v)) }
    val setNoFixDApp = dAppWithNoFix.map { case (acc, v) => TxHelpers.setScript(acc, checkNoFixDAppScript(v)) }
    val setFixDApp = dAppWithFix.map { case (acc, v) => TxHelpers.setScript(acc, checkFixDAppScript(v)) }

    val checkFix = accWithFix.map { case (acc, _) => TxHelpers.transfer(acc, recipient.toAddress, 1) }
    val checkNoFix = () => accWithNoFix.map { case (acc, _) => TxHelpers.transfer(acc, recipient.toAddress, 1) }
    val checkFixDApp = dAppWithFix.map { case (acc, _) => TxHelpers.invoke(acc.toAddress, func = None, invoker = invoker) }
    val checkNoFixDApp = () => dAppWithNoFix.map { case (acc, _) => TxHelpers.invoke(acc.toAddress, func = None, invoker = invoker) }

    (invokerBalance :: balances, setNoFix, setFix, checkFix, checkNoFix, setNoFixDApp, setFixDApp, checkFixDApp, checkNoFixDApp)
  }

  property(s"string functions return correct results for unicode input after ${BlockchainFeatures.SynchronousCalls} activation") {
    val (balances, setNoFix, setFix, checkFix, checkNoFix, setNoFixDApp, setFixDApp, checkFixDApp, checkNoFixDApp) = scenario
    withDomain(domainSettingsWithFS(fs), balances) { d =>
      val checkNoFix1     = checkNoFix()
      val checkNoFixDApp1 = checkNoFixDApp()
      d.appendBlock(setNoFix ::: setNoFixDApp: _*)

      val checkNoFixTxs = checkNoFix1 ::: checkNoFixDApp1
      d.appendBlock(checkNoFixTxs: _*)
      checkNoFixTxs.foreach(tx => d.blockchain.transactionSucceeded(tx.id.value()) shouldBe true)

      d.appendBlock()
      d.blockchain.height shouldBe activationHeight

      val checkFixTxs = checkFix ::: checkFixDApp
      d.appendBlock(setFix ::: setFixDApp: _*)
      d.appendBlock(checkFixTxs: _*)
      checkFixTxs.foreach(tx => d.blockchain.transactionSucceeded(tx.id.value()) shouldBe true)
      checkNoFix().foreach { tx =>
        d.appendBlockE(tx) should produce("TransactionNotAllowedByScript")
      }
      checkNoFixDApp().foreach { tx =>
        d.appendBlockE(tx) should produce("ScriptExecutionError(error = DApp")
      }
    }
  }
}
