package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.TestCompiler
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
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private def scenario(lastVersion: StdLibVersion) =
    for {
      recipient <- accountGen
      invoker   <- accountGen
      availableVersions     = allVersions(lastVersion)
      availableDAppVersions = allDAppVersions(lastVersion)
      accWithFix    <- Gen.listOfN(availableVersions.size, accountGen).map(_.zip(availableVersions))
      accWithNoFix  <- Gen.listOfN(versionsBeforeActivation.size, accountGen).map(_.zip(versionsBeforeActivation))
      dAppWithFix   <- Gen.listOfN(availableDAppVersions.size, accountGen).map(_.zip(availableDAppVersions))
      dAppWithNoFix <- Gen.listOfN(dAppVersionsBeforeActivation.size, accountGen).map(_.zip(dAppVersionsBeforeActivation))
      fee           <- ciFee()
      genesisTxs = (accWithFix ::: accWithNoFix ::: dAppWithFix ::: dAppWithNoFix)
        .map(a => GenesisTransaction.create(a._1.toAddress, ENOUGH_AMT, ts).explicitGet())
      invokerGenesis   = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      setNoFixVerifier = accWithNoFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkNoFixScript(a._2)), fee, ts).explicitGet())
      setFixVerifier   = accWithFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkFixScript(a._2)), fee, ts).explicitGet())
      setNoFixDApp     = dAppWithNoFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkNoFixDAppScript(a._2)), fee, ts).explicitGet())
      setFixDApp       = dAppWithFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkFixDAppScript(a._2)), fee, ts).explicitGet())
      checkFixVerifier = accWithFix.map(
        a =>
          TransferTransaction
            .selfSigned(TxVersion.V2, a._1, recipient.toAddress, Waves, 1, Waves, fee, ByteStr.empty, ts)
            .explicitGet()
      )
      checkNoFixVerifier = accWithNoFix.map(
        a =>
          TransferTransaction
            .selfSigned(TxVersion.V2, a._1, recipient.toAddress, Waves, 1, Waves, fee, ByteStr.empty, ts)
            .explicitGet()
      )
      checkFixDApp = dAppWithFix.map(
        a =>
          InvokeScriptTransaction
            .selfSigned(TxVersion.V2, invoker, a._1.toAddress, None, Nil, fee, Waves, ts)
            .explicitGet()
      )
      checkNoFixDApp = dAppWithNoFix.map(
        a =>
          InvokeScriptTransaction
            .selfSigned(TxVersion.V2, invoker, a._1.toAddress, None, Nil, fee, Waves, ts)
            .explicitGet()
      )
    } yield (
      invokerGenesis :: genesisTxs,
      setNoFixVerifier ::: setNoFixDApp,
      setFixVerifier ::: setFixDApp,
      checkNoFixVerifier ::: checkNoFixDApp,
      checkFixVerifier ::: checkFixDApp
    )

  private def assertNoFix(d: Domain): Unit = {
    val (genesisTxs, setNoFix, _, checkNoFix, _) = scenario(V5).sample.get
    d.appendBlock(genesisTxs: _*)
    d.appendBlock(setNoFix: _*)
    d.appendBlock(checkNoFix: _*)
    checkNoFix.foreach(tx => d.blockchain.transactionMeta(tx.id.value()).get._2 shouldBe true)
  }

  private def assertFix(d: Domain, lastVersion: StdLibVersion): Unit = {
    val (genesisTxs, setNoFix, setFix, checkNoFix, checkFix) = scenario(lastVersion).sample.get
    d.appendBlock(genesisTxs: _*)
    d.appendBlock(setFix: _*)
    d.appendBlock(checkFix: _*)
    checkFix.foreach(tx => d.blockchain.transactionMeta(tx.id.value()).get._2 shouldBe true)

    d.appendBlock(setNoFix: _*)
    checkNoFix.foreach { tx =>
      val error = if (tx.isInstanceOf[InvokeScriptTransaction]) "no fix error" else "TransactionNotAllowedByScript"
      (the[RuntimeException] thrownBy d.appendBlock(tx)).getMessage should include(error)
    }
  }

  property(s"string functions return correct results for unicode input after ${BlockchainFeatures.SynchronousCalls} activation") {
    withDomain(RideV4)(assertNoFix)
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V5)
      .foreach(v => withDomain(settingsForRide(v))(assertFix(_, v)))
  }
}
