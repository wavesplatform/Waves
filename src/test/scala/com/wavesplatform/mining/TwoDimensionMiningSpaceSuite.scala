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
      }

      val nonEmptySpaceGen: Gen[TwoDimensionMiningSpace] = for {
        leftMaxSize <- Gen.chooseNum(1, Int.MaxValue)
        rightMaxSize <- Gen.chooseNum(1, Int.MaxValue)
      } yield TwoDimensionMiningSpace.full(createConstSpaceEstimator(leftMaxSize), createConstSpaceEstimator(rightMaxSize))

      "should be false is both of two dimensions are non-empty" in forAll(nonEmptySpaceGen) { space =>
        space.isEmpty shouldBe false
      }
    }

    "put(block)" - {
      "should return Some if the operation is successful for both dimensions" in {
        val tank1 = new MiningSpace {
          override def isEmpty: Boolean = false
          override def put(x: Block): Option[MiningSpace] = Some(this)
          override def put(x: Transaction): Option[MiningSpace] = ???
        }

        val tank2 = new MiningSpace {
          override def isEmpty: Boolean = false
          override def put(x: Block): Option[MiningSpace] = Some(this)
          override def put(x: Transaction): Option[MiningSpace] = ???
        }

        val tank = TwoDimensionMiningSpace.partial(tank1, tank2)
        tank.put(TestBlock.create(Seq(randomTransactionGen.sample.get))) shouldBe defined
      }

      "should return None if the operation is unsuccessful for one of dimensions" - {
        val firstOverfills: Gen[(TwoDimensionMiningSpace, Seq[Block])] = for {
          leftMaxSize <- Gen.chooseNum(1, 5)
          rightMaxSize <- Gen.chooseNum(leftMaxSize + 1, leftMaxSize + 5)
          txs <- Gen.listOfN(rightMaxSize, randomTransactionGen)
        } yield {
          val space = TwoDimensionMiningSpace.full(
            createConstSpaceEstimator(leftMaxSize, blockSize = 1),
            createConstSpaceEstimator(rightMaxSize, blockSize = 1)
          )
          (space, txs.map(x => TestBlock.create(Seq(x))))
        }

        "first overfills" in forAll(firstOverfills) { case (space, blocks) =>
          val updatedSpace = blocks.foldLeft(Option(space)) {
            case (None, _) => None
            case (Some(r), x) => r.put(x)
          }
          updatedSpace shouldBe empty
        }

        val secondOverfills: Gen[(TwoDimensionMiningSpace, Seq[Block])] = firstOverfills.map { case (space, blocks) =>
          val swapped = space.copy(
            first = space.second,
            second = space.first
          )
          (swapped, blocks)
        }

        "second overfills" in forAll(secondOverfills) { case (space, blocks) =>
          val updatedSpace = blocks.foldLeft(Option(space)) {
            case (None, _) => None
            case (Some(r), x) => r.put(x)
          }
          updatedSpace shouldBe empty
        }

        val bothOverfills: Gen[(TwoDimensionMiningSpace, Seq[Block])] = firstOverfills.map { case (space, blocks) =>
          val swapped = space.copy(
            first = space.first,
            second = space.first
          )
          (swapped, blocks)
        }

        "both overfills" in forAll(bothOverfills) { case (space, blocks) =>
          val updatedSpace = blocks.foldLeft(Option(space)) {
            case (None, _) => None
            case (Some(r), x) => r.put(x)
          }
          updatedSpace shouldBe empty
        }
      }
    }

    "put(transaction)" - {
      "should return Some if the operation is successful for both dimensions" in {
        val tank1 = new MiningSpace {
          override def isEmpty: Boolean = false
          override def put(x: Block): Option[MiningSpace] = ???
          override def put(x: Transaction): Option[MiningSpace] = Some(this)
        }

        val tank2 = new MiningSpace {
          override def isEmpty: Boolean = false
          override def put(x: Block): Option[MiningSpace] = ???
          override def put(x: Transaction): Option[MiningSpace] = Some(this)
        }

        val tank = TwoDimensionMiningSpace.partial(tank1, tank2)
        tank.put(randomTransactionGen.sample.get) shouldBe defined
      }

      "should return None if the operation is unsuccessful for one of dimensions" - {
        val firstOverfills: Gen[(TwoDimensionMiningSpace, Seq[Transaction])] = for {
          leftMaxSize <- Gen.chooseNum(1, 5)
          rightMaxSize <- Gen.chooseNum(leftMaxSize + 1, leftMaxSize + 5)
          txs <- Gen.listOfN(rightMaxSize, randomTransactionGen)
        } yield {
          val space = TwoDimensionMiningSpace.full(
            createConstSpaceEstimator(leftMaxSize, transactionSize = 1),
            createConstSpaceEstimator(rightMaxSize, transactionSize = 1)
          )
          (space, txs)
        }

        "first overfills" in forAll(firstOverfills) { case (space, txs) =>
          val updatedSpace = txs.foldLeft(Option(space)) {
            case (None, _) => None
            case (Some(r), x) => r.put(x)
          }
          updatedSpace shouldBe empty
        }

        val secondOverfills: Gen[(TwoDimensionMiningSpace, Seq[Transaction])] = firstOverfills.map { case (space, txs) =>
          val swapped = space.copy(
            first = space.second,
            second = space.first
          )
          (swapped, txs)
        }

        "second overfills" in forAll(secondOverfills) { case (space, blocks) =>
          val updatedSpace = blocks.foldLeft(Option(space)) {
            case (None, _) => None
            case (Some(r), x) => r.put(x)
          }
          updatedSpace shouldBe empty
        }

        val bothOverfills: Gen[(TwoDimensionMiningSpace, Seq[Transaction])] = firstOverfills.map { case (space, txs) =>
          val swapped = space.copy(
            first = space.first,
            second = space.first
          )
          (swapped, txs)
        }

        "both overfills" in forAll(bothOverfills) { case (space, blocks) =>
          val updatedSpace = blocks.foldLeft(Option(space)) {
            case (None, _) => None
            case (Some(r), x) => r.put(x)
          }
          updatedSpace shouldBe empty
        }
      }
    }
  }

  private def createConstSpaceEstimator(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = new SpaceEstimator {
    override def max: Long = maxSize
    override implicit def estimate(x: Block): Long = blockSize
    override implicit def estimate(x: Transaction): Long = transactionSize
  }
}
