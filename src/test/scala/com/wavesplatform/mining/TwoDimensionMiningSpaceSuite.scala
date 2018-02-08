package com.wavesplatform.mining

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.Transaction

class TwoDimensionMiningSpaceSuite extends FreeSpec with Matchers with PropertyChecks with TransactionGen with NoShrink {
  "TwoDimensionMiningSpace" - {
    "isEmpty" - {
      val emptySpaceGen: Gen[TwoDimensionMiningSpace] = for {
        isLeft <- Arbitrary.arbBool.arbitrary
        isRight <- Arbitrary.arbBool.arbitrary
        if isLeft || isRight
        leftMaxSize <- if (isLeft) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- if (isRight) Gen.const(0) else Gen.chooseNum(1, Int.MaxValue)
      } yield TwoDimensionMiningSpace.full(createConstSpaceEstimator(leftMaxSize), createConstSpaceEstimator(rightMaxSize))

      "should be true if one dimension is empty" in forAll(emptySpaceGen) { space =>
        space.isEmpty shouldBe true
        space.isOverfilled shouldBe false
      }

      val nonEmptySpaceGen: Gen[TwoDimensionMiningSpace] = for {
        leftMaxSize <- Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- Gen.chooseNum(1, Int.MaxValue)
      } yield TwoDimensionMiningSpace.full(createConstSpaceEstimator(leftMaxSize), createConstSpaceEstimator(rightMaxSize))

      "should be false is both of two dimensions are non-empty" in forAll(nonEmptySpaceGen) { space =>
        space.isEmpty shouldBe false
        space.isOverfilled shouldBe false
      }
    }

    "put(block)" - tests(createConstSpaceEstimator(_, blockSize = 1)) { (initSpace, txs) =>
      val blocks = txs.map(x => TestBlock.create(Seq(x)))
      blocks.foldLeft(initSpace)(_.put(_))
    }

    "put(transaction)" - tests(createConstSpaceEstimator(_, transactionSize = 1)) { (initSpace, txs) =>
      txs.foldLeft(initSpace)(_.put(_))
    }
  }

  private def createConstSpaceEstimator(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = new SpaceEstimator {
    override def max: Long = maxSize
    override implicit def estimate(x: Block): Long = blockSize
    override implicit def estimate(x: Transaction): Long = transactionSize
  }

  private def tests(estimator: Int => SpaceEstimator)(fold: (TwoDimensionMiningSpace, Seq[Transaction]) => TwoDimensionMiningSpace): Unit = {
    "should return None if the operation is unsuccessful for one of dimensions" - {
      val noOverfillGen: Gen[TwoDimensionMiningSpace] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs <- Gen.listOfN(commonLimit - 1, randomTransactionGen)
      } yield {
        val space = TwoDimensionMiningSpace.full(estimator(commonLimit), estimator(commonLimit))
        fold(space, txs)
      }

      "no overfill" in forAll(noOverfillGen) { updatedSpace =>
        updatedSpace.isEmpty shouldBe false
        updatedSpace.isOverfilled shouldBe false

        updatedSpace.first.isEmpty shouldBe false
        updatedSpace.first.isOverfilled shouldBe false

        updatedSpace.second.isEmpty shouldBe false
        updatedSpace.second.isOverfilled shouldBe false
      }

      val firstOverfillsGen: Gen[TwoDimensionMiningSpace] = for {
        firstLimit <- Gen.chooseNum(1, 5)
        secondLimit <- Gen.chooseNum(firstLimit + 2, firstLimit + 5)
        txs <- Gen.listOfN(firstLimit + 1, randomTransactionGen)
      } yield {
        val space = TwoDimensionMiningSpace.full(estimator(firstLimit), estimator(secondLimit))
        fold(space, txs)
      }

      "first overfills" in forAll(firstOverfillsGen) { updatedSpace =>
        updatedSpace.isEmpty shouldBe true
        updatedSpace.isOverfilled shouldBe true

        updatedSpace.first.isEmpty shouldBe true
        updatedSpace.first.isOverfilled shouldBe true

        updatedSpace.second.isEmpty shouldBe false
        updatedSpace.second.isOverfilled shouldBe false
      }

      val secondOverfillsGen: Gen[TwoDimensionMiningSpace] = for {
        firstLimit <- Gen.chooseNum(3, 9)
        secondLimit <- Gen.chooseNum(1, firstLimit - 2)
        txs <- Gen.listOfN(firstLimit - 1, randomTransactionGen)
      } yield {
        val space = TwoDimensionMiningSpace.full(estimator(firstLimit), estimator(secondLimit))
        fold(space, txs)
      }

      "second overfills" in forAll(secondOverfillsGen) { updatedSpace =>
        updatedSpace.isEmpty shouldBe true
        updatedSpace.isOverfilled shouldBe true

        updatedSpace.first.isEmpty shouldBe false
        updatedSpace.first.isOverfilled shouldBe false

        updatedSpace.second.isEmpty shouldBe true
        updatedSpace.second.isOverfilled shouldBe true
      }

      val bothOverfillGen: Gen[TwoDimensionMiningSpace] = for {
        commonLimit <- Gen.chooseNum(1, 5)
        txs <- Gen.listOfN(commonLimit + 1, randomTransactionGen)
      } yield {
        val space = TwoDimensionMiningSpace.full(estimator(commonLimit), estimator(commonLimit))
        fold(space, txs)
      }

      "both overfills" in forAll(bothOverfillGen) { updatedSpace =>
        updatedSpace.isEmpty shouldBe true
        updatedSpace.isOverfilled shouldBe true

        updatedSpace.first.isEmpty shouldBe true
        updatedSpace.first.isOverfilled shouldBe true

        updatedSpace.second.isEmpty shouldBe true
        updatedSpace.second.isOverfilled shouldBe true
      }
    }
  }
}
