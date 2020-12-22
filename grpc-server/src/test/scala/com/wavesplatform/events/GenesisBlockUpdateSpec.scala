//package com.wavesplatform.events
//
//import com.wavesplatform.common.utils.EitherExt2
//import com.wavesplatform.settings.WavesSettings
//import com.wavesplatform.state.diffs.ENOUGH_AMT
//import com.wavesplatform.transaction.GenesisTransaction
//import com.wavesplatform.{BlockGen, TestHelpers}
//import org.scalacheck.Gen
//import org.scalatest.{FreeSpec, Matchers}
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//
//class GenesisBlockUpdateSpec extends FreeSpec with Matchers with BlockGen with ScalaCheckPropertyChecks with EventsHelpers {
//  override protected def settings: WavesSettings = TestHelpers.enableNG(super.settings)
//
//  val genesisAppendWithWavesAmountGen: Gen[(BlockAppended, Long)] = for {
//    master      <- accountGen
//    wavesAmount <- Gen.choose(1L, ENOUGH_AMT)
//    gt = GenesisTransaction.create(master.toAddress, wavesAmount, 0).explicitGet()
//    b <- blockGen(Seq(gt), master)
//    ba = appendBlock(b)
//  } yield (ba, wavesAmount)
//
//  "on genesis block append" - {
//    "master address balance gets correctly updated" in forAll(genesisAppendWithWavesAmountGen) {
//      case (BlockAppended(_, _, _, _, _, upds), wavesAmount) =>
//        upds.head.balances.head._3 shouldBe wavesAmount
//    }
//
//    "updated Waves amount is calculated correctly" in forAll(genesisAppendWithWavesAmountGen) {
//      case (BlockAppended(_, _, _, updatedWavesAmount, _, _), wavesAmount) =>
//        updatedWavesAmount shouldBe wavesAmount
//    }
//  }
//
//}
