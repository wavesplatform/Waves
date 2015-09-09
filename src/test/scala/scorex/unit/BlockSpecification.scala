package scorex.unit

import org.scalatest.{Matchers, FunSuite}
import scorex.TestingCommons
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.transaction.{Transaction, PaymentTransaction, SimpleTransactionModule}

import scala.util.Random

class BlockSpecification extends FunSuite with Matchers with TestingCommons {

  ignore("block generation then validation") {
  }


  test("Nxt block with txs bytes/parse roundtrip") {
    implicit val consensusModule = new NxtLikeConsensusModule()
    implicit val transactionModule = new SimpleTransactionModule()

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)

    val bt = Random.nextLong()
    val gs = Array.fill(NxtLikeConsensusModule.GeneratorSignatureLength)(Random.nextInt(100).toByte)

    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx:Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = gs
      override val baseTarget: Long = bt
    }

    val version = 1:Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    val parsedBlock = Block.parse(block.bytes).get

    assert(parsedBlock.consensusDataField.value.asInstanceOf[NxtLikeConsensusBlockData].generationSignature.sameElements(gs))
    assert(parsedBlock.versionField.value == version)
    assert(parsedBlock.signerDataField.value.generator.publicKey.sameElements(gen.publicKey))
  }


  ignore("restoring from Base58 form") {

  }
}