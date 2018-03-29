package com.wavesplatform.mining

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.Transaction

class TwoDimensionalMiningConstraintSuite extends FreeSpec with Matchers with PropertyChecks with TransactionGen with NoShrink {
  "TwoDimensionalMiningConstraint" - {
    "isEmpty" - {
      val emptyConstraintGen: Gen[TwoDimensionalMiningConstraint] = for {
        isLeft  <- Arbitrary.arbBool.arbitrary
        isRight <- Arbitrary.arbBool.arbitrary
        if isLeft || isRight
        leftMaxSize  <- if (isLeft) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- if (isRight) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
      } yield TwoDimensionalMiningConstraint.full(createConstConstraint(leftMaxSize), createConstConstraint(rightMaxSize))

      "should be true if one dimension is empty" in forAll(emptyConstraintGen) { constraint =>
        constraint.isEmpty shouldBe true
        constraint.isOverfilled shouldBe false
      }

      val nonEmptyConstraintGen: Gen[TwoDimensionalMiningConstraint] = for {
        leftMaxSize  <- Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- Gen.chooseNum(1, Int.MaxValue)
      } yield TwoDimensionalMiningConstraint.full(createConstConstraint(leftMaxSize), createConstConstraint(rightMaxSize))

      "should be false is both of two dimensions are non-empty" in forAll(nonEmptyConstraintGen) { constraint =>
        constraint.isEmpty shouldBe false
        constraint.isOverfilled shouldBe false
      }
    }

    "put(block)" - tests(createConstConstraint(_, blockSize = 1)) { (initConstraint, txs) =>
      val blocks = txs.map(x => TestBlock.create(Seq(x)))
      blocks.foldLeft(initConstraint)(_.put(_))
    }

    "put(transaction)" - tests(createConstConstraint(_, transactionSize = 1)) { (initConstraint, txs) =>
      txs.foldLeft(initConstraint)(_.put(_))
    }
  }

  private def createConstConstraint(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = new Estimator {
    override def max: Long                               = maxSize
    override implicit def estimate(x: Block): Long       = blockSize
    override implicit def estimate(x: Transaction): Long = transactionSize
  }

  private def tests(estimator: Int => Estimator)(fold: (TwoDimensionalMiningConstraint, Seq[Transaction]) => TwoDimensionalMiningConstraint): Unit = {
    "should return None if the operation is unsuccessful for one of dimensions" - {
      val noOverfillGen: Gen[TwoDimensionalMiningConstraint] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs         <- Gen.listOfN(commonLimit - 1, randomTransactionGen)
      } yield {
        val constraint = TwoDimensionalMiningConstraint.full(estimator(commonLimit), estimator(commonLimit))
        fold(constraint, txs)
      }

      "no overfill" in forAll(noOverfillGen) { updatedConstraint =>
        updatedConstraint.isEmpty shouldBe false
        updatedConstraint.isOverfilled shouldBe false

        updatedConstraint.first.isEmpty shouldBe false
        updatedConstraint.first.isOverfilled shouldBe false

        updatedConstraint.second.isEmpty shouldBe false
        updatedConstraint.second.isOverfilled shouldBe false
      }

      val firstOverfillsGen: Gen[TwoDimensionalMiningConstraint] = for {
        firstLimit  <- Gen.chooseNum(1, 5)
        secondLimit <- Gen.chooseNum(firstLimit + 2, firstLimit + 5)
        txs         <- Gen.listOfN(firstLimit + 1, randomTransactionGen)
      } yield {
        val constraint = TwoDimensionalMiningConstraint.full(estimator(firstLimit), estimator(secondLimit))
        fold(constraint, txs)
      }

      "first overfills" in forAll(firstOverfillsGen) { updatedConstraint =>
        updatedConstraint.isEmpty shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.first.isEmpty shouldBe true
        updatedConstraint.first.isOverfilled shouldBe true

        updatedConstraint.second.isEmpty shouldBe false
        updatedConstraint.second.isOverfilled shouldBe false
      }

      val secondOverfillsGen: Gen[TwoDimensionalMiningConstraint] = for {
        firstLimit  <- Gen.chooseNum(3, 9)
        secondLimit <- Gen.chooseNum(1, firstLimit - 2)
        txs         <- Gen.listOfN(firstLimit - 1, randomTransactionGen)
      } yield {
        val constraint = TwoDimensionalMiningConstraint.full(estimator(firstLimit), estimator(secondLimit))
        fold(constraint, txs)
      }

      "second overfills" in forAll(secondOverfillsGen) { updatedConstraint =>
        updatedConstraint.isEmpty shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.first.isEmpty shouldBe false
        updatedConstraint.first.isOverfilled shouldBe false

        updatedConstraint.second.isEmpty shouldBe true
        updatedConstraint.second.isOverfilled shouldBe true
      }

      val bothOverfillGen: Gen[TwoDimensionalMiningConstraint] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs         <- Gen.listOfN(commonLimit + 1, randomTransactionGen)
      } yield {
        val constraint = TwoDimensionalMiningConstraint.full(estimator(commonLimit), estimator(commonLimit))
        fold(constraint, txs)
      }

      "both overfills" in forAll(bothOverfillGen) { updatedConstraint =>
        updatedConstraint.isEmpty shouldBe true
        updatedConstraint.isOverfilled shouldBe true

        updatedConstraint.first.isEmpty shouldBe true
        updatedConstraint.first.isOverfilled shouldBe true

        updatedConstraint.second.isEmpty shouldBe true
        updatedConstraint.second.isOverfilled shouldBe true
      }
    }
  }
}
