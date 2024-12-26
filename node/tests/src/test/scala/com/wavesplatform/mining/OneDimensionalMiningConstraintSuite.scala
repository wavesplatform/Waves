package com.wavesplatform.mining

import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Transaction
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory

class OneDimensionalMiningConstraintSuite extends FreeSpec with PathMockFactory {
  "OneDimensionalMiningConstraint" - {
    "should be full if the limit is 0, but not overfilled" in {
      val tank = createConstConstraint(0, 1, "const")
      tank.isFull shouldBe true
      tank.isOverfilled shouldBe false
    }

    "put(transaction)" - tests { (maxTxs, txs) =>
      val constraint = createConstConstraint(maxTxs, transactionSize = 1, "txSize")
      txs.foldLeft(constraint)(_.put(stub[Blockchain], _, StateSnapshot.empty))
    }
  }

  private def tests(toConstraint: (Int, List[Transaction]) => MiningConstraint): Unit = {
    val dontReachLimitGen: Gen[MiningConstraint] = for {
      maxTxs   <- Gen.chooseNum(1, Int.MaxValue)
      txNumber <- Gen.chooseNum(0, maxTxs - 1)
      txs      <- Gen.listOfN(math.min(txNumber, 15), randomTransactionGen)
    } yield toConstraint(maxTxs, txs)

    "multiple items don't reach the limit" in forAll(dontReachLimitGen) { updatedConstraint =>
      updatedConstraint.isFull shouldBe false
      updatedConstraint.isOverfilled shouldBe false
    }

    val reachSoftLimitGen: Gen[MiningConstraint] = for {
      maxTxs <- Gen.chooseNum(1, 10)
      txs    <- Gen.listOfN(maxTxs, randomTransactionGen)
    } yield toConstraint(maxTxs, txs)

    "multiple items reach the limit softly" in forAll(reachSoftLimitGen) { updatedConstraint =>
      updatedConstraint.isFull shouldBe true
      updatedConstraint.isOverfilled shouldBe false
    }

    val reachHardLimitGen: Gen[MiningConstraint] = for {
      maxTxs   <- Gen.chooseNum(1, 10)
      txNumber <- Gen.chooseNum(maxTxs + 1, maxTxs + 10)
      txs      <- Gen.listOfN(txNumber, randomTransactionGen)
    } yield toConstraint(maxTxs, txs)

    "multiple items reach the limit with gap" in forAll(reachHardLimitGen) { updatedConstraint =>
      updatedConstraint.isFull shouldBe true
      updatedConstraint.isOverfilled shouldBe true
    }
  }
}
