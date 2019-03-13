package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.utils.mtree.{ProvableBytes, _}
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen
import scorex.crypto.authds.merkle.{MerkleProof, MerkleTree}
import scorex.crypto.hash.{CryptographicHash, Digest32}

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
    computeTree(st.transactions(Random.nextInt(st.transactions.length)))
  }

  @Benchmark
  def findProofForElement_test(st: St): Unit = {
    val rnd = Random.nextInt(st.transactions.length)

    val txs  = st.transactions(rnd)
    val tree = st.trees(rnd)

    txs foreach { tx =>
      assert(getProofFor(tx, tree).isDefined)
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

    lazy val trees: Vector[MerkleTree[Digest32]] = {
      println("\nGenerating test data")
      val v = transactions.map { txs =>
        computeTree(txs)
      }
      println("\nTest data generated")
      v
    }

    lazy val proofs: Vector[Vector[MerkleProof[Digest32]]] = {
      println("\nGenerating test data")
      val v = (transactions zip trees) map {
        case (txs, tree) =>
          txs.map(getProofFor(_, tree).get)
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

  implicit val cryptoHashF: CryptographicHash[Digest32] = new CryptographicHash[Digest32] {
    override val DigestSize: Int = 32

    override def hash(input: Array[Byte]): Digest32 = Digest32 @@ com.wavesplatform.crypto.secureHash(input)
  }

  implicit val txProvable: ProvableBytes[Transaction] =
    (a: Transaction) => a.bytes()
}
