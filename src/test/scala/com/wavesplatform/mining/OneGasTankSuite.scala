package com.wavesplatform.mining

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.Transaction

class OneGasTankSuite extends FreeSpec with Matchers with PropertyChecks with TransactionGen with NoShrink {
  "OneGasTank" - {
    "should be empty if the limit is 0" in {
      val tank = OneGasTank.full(createConstGasEstimator(0))
      tank.isEmpty shouldBe true
    }

    "withdraw(block)" - {
      "should return true when limit is reached softly" in {
        val tank = OneGasTank.full(createConstGasEstimator(1, blockSize = 1))
        tank.withdraw(TestBlock.create(Seq(randomTransactionGen.sample.get))) shouldBe true
      }

      "should return false when we try to withdraw more than the limit" in {
        val tank = OneGasTank.full(createConstGasEstimator(1, blockSize = 2))
        tank.withdraw(TestBlock.create(Seq(randomTransactionGen.sample.get))) shouldBe false
      }

      "should make the tank empty when it reaches the limit" in {
        val tank = OneGasTank.full(createConstGasEstimator(1, blockSize = 1))
        tank.withdraw(TestBlock.create(Seq(randomTransactionGen.sample.get)))
        tank.isEmpty shouldBe true
      }

      val dontReachLimitGen: Gen[(GasEstimator, Iterator[Block])] = for {
        max <- Gen.chooseNum(1, Int.MaxValue)
        maxTotalTxs <- Gen.chooseNum(0, max - 1)
        txs <- Gen.listOfN(math.min(maxTotalTxs, 15), randomTransactionGen)
      } yield (createConstGasEstimator(max, blockSize = 1), txs.grouped(3).map(TestBlock.create))

      "multiple transactions don't reach the limit" in forAll(dontReachLimitGen) { case (estimator, blocks) =>
        val tank = OneGasTank.full(estimator)
        blocks.foreach(tank.withdraw)
        tank.isEmpty shouldBe false
      }

      val reachLimitGen: Gen[(GasEstimator, List[Block])] = for {
        max <- Gen.chooseNum(1, 10)
        maxTotalTxs <- Gen.chooseNum(10, 20)
        txs <- Gen.listOfN(maxTotalTxs, randomTransactionGen)
      } yield (createConstGasEstimator(max, blockSize = 1), txs.map(x => TestBlock.create(Seq(x))))

      "multiple transactions reach the limit" in forAll(reachLimitGen) { case (estimator, blocks) =>
        val tank = OneGasTank.full(estimator)
        blocks.foreach(tank.withdraw)
        tank.isEmpty shouldBe true
      }
    }

    "withdraw(transaction)" - {
      "should return true when limit is reached softly" in {
        val tank = OneGasTank.full(createConstGasEstimator(1, transactionSize = 1))
        tank.withdraw(randomTransactionGen.sample.get) shouldBe true
      }

      "should return false when we try to withdraw more than the limit" in {
        val tank = OneGasTank.full(createConstGasEstimator(1, transactionSize = 2))
        tank.withdraw(randomTransactionGen.sample.get) shouldBe false
      }

      "should make the tank empty when it reaches the limit" in {
        val tank = OneGasTank.full(createConstGasEstimator(1, transactionSize = 1))
        tank.withdraw(randomTransactionGen.sample.get)
        tank.isEmpty shouldBe true
      }

      val dontReachLimitGen: Gen[(GasEstimator, List[Transaction])] = for {
        max <- Gen.chooseNum(1, Int.MaxValue)
        maxTotalTxs <- Gen.chooseNum(0, max - 1)
        txs <- Gen.listOfN(math.min(maxTotalTxs, 15), randomTransactionGen)
      } yield (createConstGasEstimator(max, transactionSize = 1), txs)

      "multiple transactions don't reach the limit" in forAll(dontReachLimitGen) { case (estimator, txs) =>
        val tank = OneGasTank.full(estimator)
        txs.foreach(tank.withdraw)
        tank.isEmpty shouldBe false
      }

      val reachLimitGen: Gen[(GasEstimator, List[Transaction])] = for {
        max <- Gen.chooseNum(1, 10)
        maxTotalTxs <- Gen.chooseNum(10, 20)
        txs <- Gen.listOfN(maxTotalTxs, randomTransactionGen)
      } yield (createConstGasEstimator(max, transactionSize = 1), txs)

      "multiple transactions reach the limit" in forAll(reachLimitGen) { case (estimator, txs) =>
        val tank = OneGasTank.full(estimator)
        txs.foreach(tank.withdraw)
        tank.isEmpty shouldBe true
      }
    }

    "copied tank should not affect the original one" in {
      val tank1 = OneGasTank.full(createConstGasEstimator(1, transactionSize = 1))
      val tank2 = tank1.copy()
      tank2.withdraw(randomTransactionGen.sample.get)
      tank1.isEmpty shouldBe false
    }
  }

  private def createConstGasEstimator(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = new GasEstimator {
    override def max: Long = maxSize
    override implicit def estimate(x: Block): Long = blockSize
    override implicit def estimate(x: Transaction): Long = transactionSize
  }
}
