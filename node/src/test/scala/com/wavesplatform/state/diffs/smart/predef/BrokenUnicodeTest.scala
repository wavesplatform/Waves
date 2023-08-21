package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import org.scalatest.EitherValues

class BrokenUnicodeTest extends PropSpec with WithDomain with EitherValues {
  import DomainPresets.*

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

  private val versionsBeforeActivation     = DirectiveDictionary[StdLibVersion].all.filter(_ < V5)
  private val dAppVersionsBeforeActivation = versionsBeforeActivation.filter(_ >= V3)

  private def allVersions(lastVersion: StdLibVersion)     = DirectiveDictionary[StdLibVersion].all.filter(_ <= lastVersion)
  private def allDAppVersions(lastVersion: StdLibVersion) = allVersions(lastVersion).filter(_ >= V3)

  private def scenario(lastVersion: StdLibVersion) = {
    val recipient = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val availableVersions     = allVersions(lastVersion)
    val availableDAppVersions = allDAppVersions(lastVersion)

    val accWithFix = (1 to availableVersions.size).map(idx => TxHelpers.signer(idx + 1)).zip(availableVersions).toList
    val accWithNoFix = (1 to versionsBeforeActivation.size).map(idx => TxHelpers.signer(idx + availableVersions.size + 1)).zip(versionsBeforeActivation).toList
    val dAppWithFix = (1 to availableDAppVersions.size).map(idx => TxHelpers.signer(idx + availableVersions.size + versionsBeforeActivation.size + 1)).zip(availableDAppVersions).toList
    val dAppWithNoFix = (1 to dAppVersionsBeforeActivation.size).map(idx => TxHelpers.signer(idx + availableVersions.size + versionsBeforeActivation.size + availableDAppVersions.size + 1)).zip(dAppVersionsBeforeActivation).toList

    val genesis = (accWithFix ::: accWithNoFix ::: dAppWithFix ::: dAppWithNoFix)
      .map { case (acc, _) => TxHelpers.genesis(acc.toAddress) }
    val invokerGenesis = TxHelpers.genesis(invoker.toAddress)

    val setNoFixVerifier     = accWithNoFix.map { case (acc, v)  => TxHelpers.setScript(acc, checkNoFixScript(v)) }
    val setFixVerifier       = accWithFix.map { case (acc, v)    => TxHelpers.setScript(acc, checkFixScript(v)) }
    val setNoFixDApp = dAppWithNoFix.map { case (acc, v) => TxHelpers.setScript(acc, checkNoFixDAppScript(v)) }
    val setFixDApp   = dAppWithFix.map { case (acc, v)   => TxHelpers.setScript(acc, checkFixDAppScript(v)) }

    val checkFixVerifier       = accWithFix.map { case (acc, _) => TxHelpers.transfer(acc, recipient.toAddress, 1) }
    val checkNoFixVerifier     = accWithNoFix.map { case (acc, _) => TxHelpers.transfer(acc, recipient.toAddress, 1) }
    val checkFixDApp = dAppWithFix.map { case (acc, _) => TxHelpers.invoke(acc.toAddress, func = None, invoker = invoker) }
    val checkNoFixDApp = dAppWithNoFix.map { case (acc, _) => TxHelpers.invoke(acc.toAddress, func = None, invoker = invoker) }

    (
      invokerGenesis :: genesis,
      setNoFixVerifier ::: setNoFixDApp,
      setFixVerifier ::: setFixDApp,
      checkNoFixVerifier ::: checkNoFixDApp,
      checkFixVerifier ::: checkFixDApp
    )
  }

  private def assertNoFix(d: Domain): Unit = {
    val (genesisTxs, setNoFix, _, checkNoFix, _) = scenario(V5)
    d.appendBlock(genesisTxs*)
    d.appendBlock(setNoFix*)
    d.appendBlock(checkNoFix*)
    checkNoFix.foreach(tx => d.blockchain.transactionSucceeded(tx.id.value()) shouldBe true)
  }

  private def assertFix(d: Domain, lastVersion: StdLibVersion): Unit = {
    val (genesisTxs, setNoFix, setFix, checkNoFix, checkFix) = scenario(lastVersion)
    d.appendBlock(genesisTxs*)
    d.appendBlock(setFix*)
    d.appendBlock(checkFix*)
    checkFix.foreach(tx => d.blockchain.transactionSucceeded(tx.id.value()) shouldBe true)

    d.appendBlock(setNoFix*)
    checkNoFix.foreach { tx =>
      val error = if (tx.isInstanceOf[InvokeScriptTransaction]) "no fix error" else "TransactionNotAllowedByScript"
      (the[RuntimeException] thrownBy d.appendBlock(tx)).getMessage should include(error)
    }
  }

  property(s"string functions return correct results for unicode input after ${BlockchainFeatures.SynchronousCalls} activation") {
    withDomain(RideV4)(assertNoFix)
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V5)
      .foreach(v => withDomain(settingsForRide(v).configure(_.copy(enforceTransferValidationAfter = 0)))(assertFix(_, v)))
  }
}
