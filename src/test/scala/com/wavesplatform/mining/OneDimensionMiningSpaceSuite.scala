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
    "should be empty if the limit is 0" in {
      val tank = OneDimensionMiningSpace.full(createConstGasEstimator(0))
      tank.isFull shouldBe true
    }

    "put(block)" - {
      "should return Some" - {
        "when the limit is not reached" in {
          val tank = OneDimensionMiningSpace.full(createConstGasEstimator(2, blockSize = 1))
          tank.put(TestBlock.create(Seq(randomTransactionGen.sample.get))) shouldBe defined
        }

        "when the imit is reached softly" in {
          val tank = OneDimensionMiningSpace.full(createConstGasEstimator(1, blockSize = 1))
          tank.put(TestBlock.create(Seq(randomTransactionGen.sample.get))) shouldBe defined
        }
      }

      "should return None when we try to fill more than the limit" in {
        val tank = OneDimensionMiningSpace.full(createConstGasEstimator(1, blockSize = 2))
        tank.put(TestBlock.create(Seq(randomTransactionGen.sample.get))) shouldBe empty
      }

      "should return empty space when it reaches the limit during put" in {
        val space = OneDimensionMiningSpace.full(createConstGasEstimator(1, blockSize = 1))
        val updatedSpace = space.put(TestBlock.create(Seq(randomTransactionGen.sample.get)))
        updatedSpace shouldBe defined
        updatedSpace.get.isFull shouldBe true
      }

      val dontReachLimitGen: Gen[(SpaceEstimator, Iterator[Block])] = for {
        max <- Gen.chooseNum(1, Int.MaxValue)
        maxTotalTxs <- Gen.chooseNum(0, max - 1)
        txs <- Gen.listOfN(math.min(maxTotalTxs, 15), randomTransactionGen)
      } yield (createConstGasEstimator(max, blockSize = 1), txs.grouped(3).map(TestBlock.create))

      "multiple transactions don't reach the limit" in forAll(dontReachLimitGen) { case (estimator, blocks) =>
        val space = OneDimensionMiningSpace.full(estimator)
        val updatedSpace = blocks.foldLeft(Option(space)) {
          case (None, _) => None
          case (Some(r), x) => r.put(x)
        }
        updatedSpace shouldBe defined
        updatedSpace.get.isFull shouldBe false
      }

      val reachLimitGen: Gen[(SpaceEstimator, List[Block])] = for {
        max <- Gen.chooseNum(1, 10)
        maxTotalTxs <- Gen.chooseNum(10, 20)
        txs <- Gen.listOfN(maxTotalTxs, randomTransactionGen)
      } yield (createConstGasEstimator(max, blockSize = 1), txs.map(x => TestBlock.create(Seq(x))))

      "multiple transactions reach the limit" in forAll(reachLimitGen) { case (estimator, blocks) =>
        val space = OneDimensionMiningSpace.full(estimator)
        val updatedSpace = blocks.foldLeft(Option(space)) {
          case (None, _) => None
          case (Some(r), x) => r.put(x)
        }

        updatedSpace match {
          case None =>
          case Some(x) =>
            x.isFull shouldBe true
        }
      }
    }

    "put(transaction)" - {
      "should return Some" - {
        "when the limit is not reached" in {
          val tank = OneDimensionMiningSpace.full(createConstGasEstimator(2, transactionSize = 1))
          tank.put(randomTransactionGen.sample.get) shouldBe defined
        }

        "when the imit is reached softly" in {
          val tank = OneDimensionMiningSpace.full(createConstGasEstimator(1, transactionSize = 1))
          tank.put(randomTransactionGen.sample.get) shouldBe defined
        }
      }

      "should return None when we try to fill more than the limit" in {
        val tank = OneDimensionMiningSpace.full(createConstGasEstimator(1, transactionSize = 2))
        tank.put(randomTransactionGen.sample.get) shouldBe empty
      }

      "should return empty space when it reaches the limit during put" in {
        val space = OneDimensionMiningSpace.full(createConstGasEstimator(1, transactionSize = 1))
        val updatedSpace = space.put(randomTransactionGen.sample.get)
        updatedSpace shouldBe defined
        updatedSpace.get.isFull shouldBe true
      }

      val dontReachLimitGen: Gen[(SpaceEstimator, List[Transaction])] = for {
        max <- Gen.chooseNum(1, Int.MaxValue)
        maxTotalTxs <- Gen.chooseNum(0, max - 1)
        txs <- Gen.listOfN(math.min(maxTotalTxs, 15), randomTransactionGen)
      } yield (createConstGasEstimator(max, transactionSize = 1), txs)

      "multiple transactions don't reach the limit" in forAll(dontReachLimitGen) { case (estimator, blocks) =>
        val space = OneDimensionMiningSpace.full(estimator)
        val updatedSpace = blocks.foldLeft(Option(space)) {
          case (None, _) => None
          case (Some(r), x) => r.put(x)
        }
        updatedSpace shouldBe defined
        updatedSpace.get.isFull shouldBe false
      }

      val reachLimitGen: Gen[(SpaceEstimator, List[Transaction])] = for {
        max <- Gen.chooseNum(1, 10)
        maxTotalTxs <- Gen.chooseNum(10, 20)
        txs <- Gen.listOfN(maxTotalTxs, randomTransactionGen)
      } yield (createConstGasEstimator(max, transactionSize = 1), txs)

      "multiple transactions reach the limit" in forAll(reachLimitGen) { case (estimator, txs) =>
        val space = OneDimensionMiningSpace.full(estimator)
        val updatedSpace = txs.foldLeft(Option(space)) {
          case (None, _) => None
          case (Some(r), x) => r.put(x)
        }

        updatedSpace match {
          case None =>
          case Some(x) =>
            x.isFull shouldBe true
        }
      }
    }

    "copied tank should not affect the original one" in {
      val tank1 = OneDimensionMiningSpace.full(createConstGasEstimator(1, transactionSize = 1))
      val tank2 = tank1.copy()
      tank2.put(randomTransactionGen.sample.get)
      tank1.isFull shouldBe false
    }
  }

  private def createConstGasEstimator(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = new SpaceEstimator {
    override def max: Long = maxSize
    override implicit def estimate(x: Block): Long = blockSize
    override implicit def estimate(x: Transaction): Long = transactionSize
  }
}
