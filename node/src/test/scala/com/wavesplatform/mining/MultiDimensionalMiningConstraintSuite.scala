package com.wavesplatform.mining

import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Transaction
import org.scalacheck.{Arbitrary, Gen}

class MultiDimensionalMiningConstraintSuite extends FreeSpec {
  "MultiDimensionalMiningConstraint" - {
    "isFull" - {
      val emptyConstraintGen: Gen[MultiDimensionalMiningConstraint] = for {
        isLeft  <- Arbitrary.arbBool.arbitrary
        isRight <- Arbitrary.arbBool.arbitrary
        if isLeft || isRight
        leftMaxSize  <- if (isLeft) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- if (isRight) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
      } yield MultiDimensionalMiningConstraint(
        createConstConstraint(leftMaxSize, 1, "leftMaxSize"),
        createConstConstraint(rightMaxSize, 1, "rightMaxSize")
      )

      "should be true if one dimension is full" in forAll(emptyConstraintGen) { constraint =>
        constraint.isFull shouldBe true
        constraint.isOverfilled should not be true
      }

      val nonEmptyConstraintGen: Gen[MultiDimensionalMiningConstraint] = for {
        leftMaxSize  <- Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- Gen.chooseNum(1, Int.MaxValue)
      } yield MultiDimensionalMiningConstraint(
        createConstConstraint(leftMaxSize, 1, "leftMaxSize"),
        createConstConstraint(rightMaxSize, 1, "rightMaxSize")
      )

      "should be false is both of two dimensions are not full" in forAll(nonEmptyConstraintGen) { constraint =>
        constraint.isFull should not be true
        constraint.isOverfilled should not be true
      }
    }

    "put(transaction)" - tests(createConstConstraint(_, transactionSize = 1, "txSize")) { (initConstraint, txs) =>
      txs.foldLeft(initConstraint)(_.put(null, _, StateSnapshot.empty))
    }
  }

  private def tests(
      estimator: Int => MiningConstraint
  )(fold: (MultiDimensionalMiningConstraint, Seq[Transaction]) => MultiDimensionalMiningConstraint): Unit = {
    "should return None if the operation is unsuccessful for one of dimensions" - {
      val noOverfillGen: Gen[MultiDimensionalMiningConstraint] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs         <- Gen.listOfN(commonLimit - 1, randomTransactionGen)
      } yield {
        val constraint = MultiDimensionalMiningConstraint(estimator(commonLimit), estimator(commonLimit))
        fold(constraint, txs)
      }

      "no overfill" in forAll(noOverfillGen) { updatedConstraint =>
        updatedConstraint.isFull should not be true
        updatedConstraint.isOverfilled should not be true

        updatedConstraint.constraints.map { x =>
          x.isFull should not be true
          x.isOverfilled should not be true
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
        updatedConstraint.isFull shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.constraints.head.isFull shouldBe true
        updatedConstraint.constraints.head.isOverfilled shouldBe true

        updatedConstraint.constraints.tail.map { x =>
          x.isFull should not be true
          x.isOverfilled should not be true
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
        updatedConstraint.isFull shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.constraints.head.isFull should not be true
        updatedConstraint.constraints.head.isOverfilled should not be true

        updatedConstraint.constraints.tail.map { x =>
          x.isFull shouldBe true
          x.isOverfilled shouldBe true
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
        updatedConstraint.isFull shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.constraints.map { x =>
          x.isFull shouldBe true
          x.isOverfilled shouldBe true
        }
      }
    }
  }
}
