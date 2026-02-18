package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position

class EndorsementFilterSpec extends FreeSpec {
  "takes with higher balance first" in {
    val filter = EndorsementFilter(
      maxValidEndorsers = 10,
      miner = GeneratorIndex(15),
      isMiner = true,
      finalizedId = TxHelpers.randomBlockId,
      finalizedHeight = Height(1),
      endorsedId = TxHelpers.randomBlockId,
      normalizedGeneratorSet = Vector(
        mkItem(0, 239130000000L),
        mkItem(1, 239510000000L),
        mkItem(2, 239730000000L),
        mkItem(3, 240320000000L),
        mkItem(4, 240531660000L),
        mkItem(5, 239270000000L),
        mkItem(6, 241149180000L),
        mkItem(7, 240310000000L),
        mkItem(8, 239230000000L),
        mkItem(9, 240330000000L),
        mkItem(10, 240670780000L),
        mkItem(11, 239230000000L),
        mkItem(12, 239230000000L),
        mkItem(13, 240570000000L),
        mkItem(14, 11823820170545L),
        mkItem(15, 10315750064535L),
        mkItem(16, 11257534317006L),
        mkItem(17, 8125473001579L)
      ),
      conflict = Set.empty
    )

    val r = filter.simulate(0 to 17, Set.empty)
    r.chosenValid.map(_.toInt) shouldBe Seq(14, 16)
    r.endorsedBalance shouldBe BigInt(33397104552086L)
  }

  "small balances, miner endorsement (impossible)" in {
    val filter = EndorsementFilter(
      maxValidEndorsers = 8,
      miner = GeneratorIndex(1),
      isMiner = true,
      finalizedId = TxHelpers.randomBlockId,
      finalizedHeight = Height(1),
      endorsedId = TxHelpers.randomBlockId,
      normalizedGeneratorSet = Vector(
        mkItem(0, 4996038000000L),
        mkItem(1, 4989990000000L),
        mkItem(2, 4989990000000L),
        mkItem(3, 4989990000000L),
        mkItem(4, 4989990000000L),
        mkItem(5, 4989990000000L),
        mkItem(6, 4989990000000L),
        mkItem(7, 4989990000000L),
        mkItem(8, 4989990000000L),
        mkItem(9, 4989990000000L),
        mkItem(10, 199989990000000L),
        mkItem(11, 199989990000000L)
      ),
      conflict = Set.empty
    )

    val r = filter.simulate(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Set.empty)
    r.endorsedBalance shouldBe BigInt(44915958000000L)
  }

  "takes only maxValidEndorsers" in {
    val filter = EndorsementFilter(
      maxValidEndorsers = 5,
      miner = GeneratorIndex(1),
      isMiner = true,
      finalizedId = TxHelpers.randomBlockId,
      finalizedHeight = Height(1),
      endorsedId = TxHelpers.randomBlockId,
      normalizedGeneratorSet = (0 to 7).map(mkItem(_, 125000000000L)).toVector,
      conflict = Set.empty
    )

    val r = filter.simulate(0 to 7, Set.empty)
    r.chosenValid.size shouldBe filter.maxValidEndorsers
  }

  private def mkItem(i: Int, balance: Long): (Address, BlsPublicKey, Long) = {
    val kp = TxHelpers.signer(i)
    (kp.toAddress, BlsKeyPair(kp.privateKey).publicKey, balance)
  }
}
