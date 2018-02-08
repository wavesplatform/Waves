package com.wavesplatform.mining

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.Transaction

class OneDimensionMiningSpaceSuite extends FreeSpec with Matchers with PropertyChecks with TransactionGen with NoShrink {
  "OneDimensionMining" - {
    "should be empty if the limit is 0, but not overfilled" in {
      val tank = OneDimensionMiningSpace.full(createConstSpaceEstimator(0))
      tank.isEmpty shouldBe true
      tank.isOverfilled shouldBe false
    }

    "put(block)" - tests { (maxTxs, txs) =>
      val estimator = createConstSpaceEstimator(maxTxs, blockSize = 1)
      val blocks = txs.map(tx => TestBlock.create(Seq(tx)))
      val space = OneDimensionMiningSpace.full(estimator)
      blocks.foldLeft(space)(_.put(_))
    }

    "put(transaction)" - tests { (maxTxs, txs) =>
      val estimator = createConstSpaceEstimator(maxTxs, transactionSize = 1)
      val space = OneDimensionMiningSpace.full(estimator)
      txs.foldLeft(space)(_.put(_))
    }
  }

  private def createConstSpaceEstimator(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = new SpaceEstimator {
    override def max: Long = maxSize
    override implicit def estimate(x: Block): Long = blockSize
    override implicit def estimate(x: Transaction): Long = transactionSize
  }

  private def tests(toSpace: (Int, List[Transaction]) => MiningSpace): Unit = {
    val dontReachLimitGen: Gen[MiningSpace] = for {
      maxTxs <- Gen.chooseNum(1, Int.MaxValue)
      txNumber <- Gen.chooseNum(0, maxTxs - 1)
      txs <- Gen.listOfN(math.min(txNumber, 15), randomTransactionGen)
    } yield toSpace(maxTxs, txs)

    "multiple items don't reach the limit" in forAll(dontReachLimitGen) { updatedSpace =>
      updatedSpace.isEmpty shouldBe false
      updatedSpace.isOverfilled shouldBe false
    }

    val reachSoftLimitGen: Gen[MiningSpace] = for {
      maxTxs <- Gen.chooseNum(1, 10)
      txs <- Gen.listOfN(maxTxs, randomTransactionGen)
    } yield toSpace(maxTxs, txs)

    "multiple items reach the limit softly" in forAll(reachSoftLimitGen) { updatedSpace =>
      updatedSpace.isEmpty shouldBe true
      updatedSpace.isOverfilled shouldBe false
    }

    val reachHardLimitGen: Gen[MiningSpace] = for {
      maxTxs <- Gen.chooseNum(1, 10)
      txNumber <- Gen.chooseNum(maxTxs + 1, maxTxs + 10)
      txs <- Gen.listOfN(txNumber, randomTransactionGen)
    } yield toSpace(maxTxs, txs)

    "multiple items reach the limit with gap" in forAll(reachHardLimitGen) { updatedSpace =>
      updatedSpace.isEmpty shouldBe true
      updatedSpace.isOverfilled shouldBe true
    }
  }
}
