package com.wavesplatform.mining

import com.wavesplatform.state.Blockchain
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
    "isEmpty" - {
      val emptyConstraintGen: Gen[MultiDimensionalMiningConstraint] = for {
        isLeft  <- Arbitrary.arbBool.arbitrary
        isRight <- Arbitrary.arbBool.arbitrary
        if isLeft || isRight
        leftMaxSize  <- if (isLeft) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- if (isRight) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
      } yield MultiDimensionalMiningConstraint(createConstConstraint(leftMaxSize, 1), createConstConstraint(rightMaxSize, 1))

      "should be true if one dimension is empty" in forAll(emptyConstraintGen) { constraint =>
        constraint.isEmpty shouldBe true
        constraint.isOverfilled shouldBe false
      }

      val nonEmptyConstraintGen: Gen[MultiDimensionalMiningConstraint] = for {
        leftMaxSize  <- Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- Gen.chooseNum(1, Int.MaxValue)
      } yield MultiDimensionalMiningConstraint(createConstConstraint(leftMaxSize, 1), createConstConstraint(rightMaxSize, 1))

      "should be false is both of two dimensions are non-empty" in forAll(nonEmptyConstraintGen) { constraint =>
        constraint.isEmpty shouldBe false
        constraint.isOverfilled shouldBe false
      }
    }

    "put(transaction)" - tests(createConstConstraint(_, transactionSize = 1)) { (initConstraint, txs) =>
      txs.foldLeft(initConstraint)(_.put(stub[Blockchain], _))
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
        updatedConstraint.isEmpty shouldBe false
        updatedConstraint.isOverfilled shouldBe false

        updatedConstraint.constraints.map { x =>
          x.isEmpty shouldBe false
          x.isOverfilled shouldBe false
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
        updatedConstraint.isEmpty shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.constraints.head.isEmpty shouldBe true
        updatedConstraint.constraints.head.isOverfilled shouldBe true

        updatedConstraint.constraints.tail.map { x =>
          x.isEmpty shouldBe false
          x.isOverfilled shouldBe false
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
        updatedConstraint.isEmpty shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.constraints.head.isEmpty shouldBe false
        updatedConstraint.constraints.head.isOverfilled shouldBe false

        updatedConstraint.constraints.tail.map { x =>
          x.isEmpty shouldBe true
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
        updatedConstraint.isEmpty shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.constraints.map { x =>
          x.isEmpty shouldBe true
          x.isOverfilled shouldBe true
        }
      }
    }
  }
}
