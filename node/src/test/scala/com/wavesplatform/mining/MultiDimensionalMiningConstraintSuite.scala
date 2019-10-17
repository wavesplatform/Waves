package com.wavesplatform.mining

import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MultiDimensionalMiningConstraintSuite
    extends FreeSpec
    with Matchers
    with PropertyChecks
    with PathMockFactory
    with TransactionGen
    with NoShrink {
  "MultiDimensionalMiningConstraint" - {
    "isFull" - {
      val emptyConstraintGen: Gen[MultiDimensionalMiningConstraint] = for {
        isLeft  <- Arbitrary.arbBool.arbitrary
        isRight <- Arbitrary.arbBool.arbitrary
        if isLeft || isRight
        leftMaxSize  <- if (isLeft) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- if (isRight) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
      } yield MultiDimensionalMiningConstraint(createConstConstraint(leftMaxSize, 1, "leftMaxSize"), createConstConstraint(rightMaxSize, 1, "rightMaxSize"))

      "should be true if one dimension is full" in forAll(emptyConstraintGen) { constraint =>
        constraint shouldBe 'full
        constraint should not be 'overfilled
      }

      val nonEmptyConstraintGen: Gen[MultiDimensionalMiningConstraint] = for {
        leftMaxSize  <- Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- Gen.chooseNum(1, Int.MaxValue)
      } yield MultiDimensionalMiningConstraint(createConstConstraint(leftMaxSize, 1, "leftMaxSize"), createConstConstraint(rightMaxSize, 1, "rightMaxSize"))

      "should be false is both of two dimensions are not full" in forAll(nonEmptyConstraintGen) { constraint =>
        constraint should not be 'full
        constraint should not be 'overfilled
      }
    }

    "put(transaction)" - tests(createConstConstraint(_, transactionSize = 1, "txSize")) { (initConstraint, txs) =>
      txs.foldLeft(initConstraint)(_.put(stub[Blockchain], _, Diff.empty))
    }
  }

  private def tests(estimator: Int => MiningConstraint)(
      fold: (MultiDimensionalMiningConstraint, Seq[Transaction]) => MultiDimensionalMiningConstraint): Unit = {
    "should return None if the operation is unsuccessful for one of dimensions" - {
      val noOverfillGen: Gen[MultiDimensionalMiningConstraint] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs         <- Gen.listOfN(commonLimit - 1, randomTransactionGen)
      } yield {
        val constraint = MultiDimensionalMiningConstraint(estimator(commonLimit), estimator(commonLimit))
        fold(constraint, txs)
      }

      "no overfill" in forAll(noOverfillGen) { updatedConstraint =>
        updatedConstraint should not be 'full
        updatedConstraint should not be 'overfilled

        updatedConstraint.constraints.map { x =>
          x should not be 'full
          x should not be 'overfilled
        }
      }

      val firstOverfillsGen: Gen[MultiDimensionalMiningConstraint] = for {
        firstLimit  <- Gen.chooseNum(1, 5)
        secondLimit <- Gen.chooseNum(firstLimit + 2, firstLimit + 5)
        txs         <- Gen.listOfN(firstLimit + 1, randomTransactionGen)
      } yield {
        val constraint = MultiDimensionalMiningConstraint(estimator(firstLimit), estimator(secondLimit))
        fold(constraint, txs)
      }

      "first overfills" in forAll(firstOverfillsGen) { updatedConstraint =>
        updatedConstraint shouldBe 'full
        updatedConstraint shouldBe 'overfilled

        updatedConstraint.constraints.head shouldBe 'full
        updatedConstraint.constraints.head shouldBe 'overfilled

        updatedConstraint.constraints.tail.map { x =>
          x should not be 'full
          x should not be 'overfilled
        }
      }

      val secondOverfillsGen: Gen[MultiDimensionalMiningConstraint] = for {
        firstLimit  <- Gen.chooseNum(3, 9)
        secondLimit <- Gen.chooseNum(1, firstLimit - 2)
        txs         <- Gen.listOfN(firstLimit - 1, randomTransactionGen)
      } yield {
        val constraint = MultiDimensionalMiningConstraint(estimator(firstLimit), estimator(secondLimit))
        fold(constraint, txs)
      }

      "tail overfills" in forAll(secondOverfillsGen) { updatedConstraint =>
        updatedConstraint shouldBe 'full
        updatedConstraint shouldBe 'overfilled

        updatedConstraint.constraints.head should not be 'full
        updatedConstraint.constraints.head should not be 'overfilled

        updatedConstraint.constraints.tail.map { x =>
          x shouldBe 'full
          x shouldBe 'overfilled
        }
      }

      val bothOverfillGen: Gen[MultiDimensionalMiningConstraint] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs         <- Gen.listOfN(commonLimit + 1, randomTransactionGen)
      } yield {
        val constraint = MultiDimensionalMiningConstraint(estimator(commonLimit), estimator(commonLimit))
        fold(constraint, txs)
      }

      "all overfills" in forAll(bothOverfillGen) { updatedConstraint =>
        updatedConstraint shouldBe 'full
        updatedConstraint shouldBe 'overfilled

        updatedConstraint.constraints.map { x =>
          x shouldBe 'full
          x shouldBe 'overfilled
        }
      }
    }
  }
}
