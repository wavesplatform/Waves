package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.utils.Merkle._
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen

import scala.util.Random

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class MerkleTreeBenchmark {

  import MerkleTreeBenchmark._

  @Benchmark
  def computeTree_test(st: St): Unit = {
    mkTxTree(st.transactions(Random.nextInt(st.transactions.length)))
  }

  @Benchmark
  def findProofForElement_test(st: St): Unit = {
    val rnd = Random.nextInt(st.transactions.length)

    val txs  = st.transactions(rnd)
    val tree = st.trees(rnd)

    txs foreach { tx =>
      assert(tree.getProofForTx(tx).isDefined)
    }
  }

  @Benchmark
  def checkProofForElement(st: St): Unit = {
    val rnd = Random.nextInt(st.transactions.length)

    val tree   = st.trees(rnd)
    val proofs = st.proofs(rnd)

    proofs foreach { pr =>
      assert(pr.valid(tree.rootHash))
    }

  }

}

object MerkleTreeBenchmark {
  @State(Scope.Benchmark)
  class St extends BaseState {

    lazy val transactions: Vector[Vector[Transaction]] = {
      println("\nGenerating test data")
      val v = Vector.fill(10)(genTxSequence(6000))
      println("\nTest data generated")
      v
    }

    lazy val trees: Vector[TxTree] = {
      println("\nGenerating test data")
      val v = transactions.map { txs =>
        mkTxTree(txs)
      }
      println("\nTest data generated")
      v
    }

    lazy val proofs: Vector[Vector[TxProof]] = {
      println("\nGenerating test data")
      val v = (transactions zip trees) map {
        case (txs, tree) =>
          txs.map(tree.getProofForTx(_).get)
      }
      println("\nTest data generated")
      v
    }

    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        amount    <- Gen.choose(1, waves(1))
        recipient <- accountGen
      } yield TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, ts, Waves, 100000, Array.emptyByteArray).explicitGet()

    def genTxSequence(count: Int): Vector[Transaction] = {
      Gen
        .sequence[Vector[Transaction], Transaction]((1 to count).map { _ =>
          txGenP(richAccount, System.currentTimeMillis())
        })
        .sample
        .get
    }
  }
}
