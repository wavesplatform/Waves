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
import com.wavesplatform.transaction.smart.SetScriptTransaction
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

  private val checkNoFixV1V2 =
    """
       | {-# CONTENT_TYPE EXPRESSION #-}
       | {-# SCRIPT_TYPE  ACCOUNT    #-}
       |
       | take("x冬x", 2)      == "x\ud87e" &&
       | size("x冬x")         == 4         &&
       | drop("x冬x", 2)      == "\udc1ax" &&
       | takeRight("x冬x", 2) == "\udc1ax" &&
       | dropRight("x冬x", 2) == "x\ud87e"
     """.stripMargin

  private val checkFixV1V2 =
    """
       | {-# CONTENT_TYPE EXPRESSION #-}
       | {-# SCRIPT_TYPE  ACCOUNT    #-}
       |
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
       | "aaa冬bbb".indexOf("\ud87e")        == 3 &&
       | "aaa冬bbb".indexOf("\ud87e", 2)     == 3 &&
       | "aaa冬bbb".lastIndexOf("\ud87e")    == 3 &&
       | "aaa冬bbb".lastIndexOf("\ud87e", 5) == 3 &&
       |
       |  "x冬x".split("").size()   == 4                                                                   &&
       |  "冬x🤦冬".split("")       == [ "\ud87e", "\udc1a", "x", "\ud83e", "\udd26", "\ud87e", "\udc1a" ] &&
       |  "冬x🤦冬".split("\ud87e") == [ "", "\udc1ax🤦", "\udc1a"]
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
       | "aaa冬bbb".indexOf("\ud87e")        == unit &&
       | "aaa冬bbb".indexOf("\ud87e", 2)     == unit &&
       | "aaa冬bbb".lastIndexOf("\ud87e")    == unit &&
       | "aaa冬bbb".lastIndexOf("\ud87e", 5) == unit &&
       |
       | "x冬x".split("").size()   == 3                        &&
       | "冬x🤦冬".split("")       == [ "冬", "x", "🤦", "冬" ] &&
       | "冬x🤦冬".split("\ud87e") == [ "冬x🤦冬" ]
     """.stripMargin

  private val checkNoFixV4 =
    s"""
       | $checkNoFixV3 &&
       | "x冬x".contains("x\ud87e")
     """.stripMargin

  private val checkFixV4AndNext =
    s"""
       | $checkFixV3 &&
       | !"x冬x".contains("x\ud87e")
     """.stripMargin

  private def checkNoFixScript(v: StdLibVersion) = TestCompiler(v).compileExpression(
    v match {
      case V1 | V2 => checkNoFixV1V2
      case V3      => checkNoFixV3
      case V4      => checkNoFixV4
      case _       => throw new RuntimeException(s"Unexpected $v")
    }
  )

  private def checkFixScript(v: StdLibVersion) = TestCompiler(v).compileExpression(
    v match {
      case V1 | V2 => checkFixV1V2
      case V3      => checkFixV3
      case _       => checkFixV4AndNext
    }
  )

  private val allVersions              = DirectiveDictionary[StdLibVersion].all
  private val versionsBeforeActivation = allVersions.filter(_ < V5)

  private val scenario =
    for {
      recipient    <- accountGen
      accWithFix   <- Gen.listOfN(allVersions.size, accountGen).map(_.zip(allVersions))
      accWithNoFix <- Gen.listOfN(versionsBeforeActivation.size, accountGen).map(_.zip(versionsBeforeActivation))
      fee          <- ciFee()
      genesisTxs = (accWithFix ::: accWithNoFix).map(a => GenesisTransaction.create(a._1.toAddress, ENOUGH_AMT, ts).explicitGet())
      setNoFix   = accWithNoFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkNoFixScript(a._2)), fee, ts).explicitGet())
      setFix     = accWithFix.map(a => SetScriptTransaction.selfSigned(1.toByte, a._1, Some(checkFixScript(a._2)), fee, ts).explicitGet())
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
    } yield (genesisTxs, setNoFix, setFix, checkFix, checkNoFix)

  property(s"string functions return correct results for unicode input after ${BlockchainFeatures.SynchronousCalls} activation") {
    val (genesisTxs, setNoFix, setFix, checkFix, checkNoFix) = scenario.sample.get
    withDomain(domainSettingsWithFS(fs)) { d =>
      d.appendBlock(genesisTxs: _*)

      val checkNoFix1 = checkNoFix()
      d.appendBlock(setNoFix: _*)
      d.appendBlock(checkNoFix1: _*)
      checkNoFix1.foreach(tx => d.blockchain.transactionMeta(tx.id.value()).get._2 shouldBe true)

      d.appendBlock()
      d.blockchain.height shouldBe activationHeight

      d.appendBlock(setFix: _*)
      d.appendBlock(checkFix: _*)
      checkFix.foreach(tx => d.blockchain.transactionMeta(tx.id.value()).get._2 shouldBe true)
      checkNoFix().foreach { tx =>
        (the[RuntimeException] thrownBy d.appendBlock(tx)).getMessage should include("TransactionNotAllowedByScript")
      }
    }
  }
}
