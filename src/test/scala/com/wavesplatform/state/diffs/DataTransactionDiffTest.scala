package com.wavesplatform.state.diffs

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry}
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DataTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.DataTransaction.id -> 0))

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  def data(version: Byte, sender: PrivateKeyAccount, data: List[DataEntry[_]], fee: Long, timestamp: Long): DataTransaction =
    DataTransaction.selfSigned(version, sender, data, fee, timestamp).explicitGet()

  property("state invariants hold") {
    val setup = for {
      (genesis, master, ts) <- baseSetup

      key1   <- validAliasStringGen
      value1 <- positiveLongGen
      item1 = IntegerDataEntry(key1, value1)
      fee1     <- smallFeeGen
      version1 <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      dataTx1 = data(version1, master, List(item1), fee1, ts + 10000)

      key2   <- validAliasStringGen
      value2 <- Arbitrary.arbitrary[Boolean]
      item2 = BooleanDataEntry(key2, value2)
      fee2     <- smallFeeGen
      version2 <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      dataTx2 = data(version2, master, List(item2), fee2, ts + 20000)

      value3 <- positiveLongGen
      item3 = IntegerDataEntry(key1, value3)
      fee3     <- smallFeeGen
      version3 <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      dataTx3 = data(version3, master, List(item3), fee3, ts + 30000)
    } yield (genesis, Seq(item1, item2, item3), Seq(dataTx1, dataTx2, dataTx3))

    forAll(setup) {
      case (genesisTx, items, txs) =>
        val sender  = txs.head.sender
        val genesis = block(Seq(genesisTx))
        val blocks  = txs.map(tx => block(Seq(tx)))

        val item1 = items.head
        assertDiffAndState(Seq(genesis), blocks(0), fs) {
          case (totalDiff, state) =>
            assertBalanceInvariant(totalDiff)
            state.portfolio(sender).balance shouldBe (ENOUGH_AMT - txs(0).fee)
            state.accountData(sender, item1.key) shouldBe Some(item1)
            state.accountData(sender).data.get(item1.key) shouldBe Some(item1)
        }

        val item2 = items(1)
        assertDiffAndState(Seq(genesis, blocks(0)), blocks(1), fs) {
          case (totalDiff, state) =>
            assertBalanceInvariant(totalDiff)
            state.portfolio(sender).balance shouldBe (ENOUGH_AMT - txs.take(2).map(_.fee).sum)
            state.accountData(sender, item1.key) shouldBe Some(item1)
            state.accountData(sender).data.get(item1.key) shouldBe Some(item1)
            state.accountData(sender, item2.key) shouldBe Some(item2)
            state.accountData(sender).data.get(item2.key) shouldBe Some(item2)
        }

        val item3 = items(2)
        assertDiffAndState(Seq(genesis, blocks(0), blocks(1)), blocks(2), fs) {
          case (totalDiff, state) =>
            assertBalanceInvariant(totalDiff)
            state.portfolio(sender).balance shouldBe (ENOUGH_AMT - txs.map(_.fee).sum)
            state.accountData(sender, item1.key) shouldBe Some(item3)
            state.accountData(sender).data.get(item1.key) shouldBe Some(item3)
            state.accountData(sender, item2.key) shouldBe Some(item2)
            state.accountData(sender).data.get(item2.key) shouldBe Some(item2)
        }
    }
  }

  property("cannot overspend funds") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      key                   <- validAliasStringGen
      value                 <- bytes64gen
      feeOverhead           <- Gen.choose[Long](1, ENOUGH_AMT)
      version               <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      dataTx = data(version, master, List(BinaryDataEntry(key, ByteStr(value))), ENOUGH_AMT + feeOverhead, ts + 10000)
    } yield (genesis, dataTx)

    forAll(setup) {
      case (genesis, dataTx) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(dataTx)), fs) { blockDiffEi =>
          blockDiffEi should produce("negative waves balance")
        }
    }
  }

  property("validation fails prior to feature activation") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      fee                   <- smallFeeGen
      version               <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      dataTx = data(version, master, List(), fee, ts + 10000)
    } yield (genesis, dataTx)
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.DataTransaction.id -> 10))

    forAll(setup) {
      case (genesis, data) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(data)), settings) { blockDiffEi =>
          blockDiffEi should produce("DataTransaction has not been activated")
        }
    }
  }
}
