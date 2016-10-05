package scorex.lagonaki.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.consensus.qora.{QoraLikeConsensusBlockData, QoraLikeConsensusModule}
import scorex.crypto.EllipticCurveImpl
import scorex.lagonaki.TestingCommons
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class BlockSpecification extends FunSuite with Matchers with TestingCommons {

  test("Nxt block with txs bytes/parse roundtrip") {
    implicit val consensusModule = new NxtLikeConsensusModule()
    implicit val transactionModule = new SimpleTransactionModule()(application.settings, application)

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)

    val bt = Random.nextLong()
    val gs = Array.fill(NxtLikeConsensusModule.GeneratorSignatureLength)(Random.nextInt(100).toByte)


    val ts = System.currentTimeMillis() - 5000
    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, ts)
    val tr: TransferTransaction = TransferTransaction.create(None, sender, gen, 5, ts + 1, None, 2, Array())
    val assetId = Some(Array.fill(EllipticCurveImpl.SignatureLength)(Random.nextInt(100).toByte))
    val tr2: TransferTransaction = TransferTransaction.create(assetId, sender, gen, 5, ts + 2, None, 2, Array())

    val tbd = Seq(tx, tr, tr2)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = gs
      override val baseTarget: Long = bt
    }

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    val parsedBlock = Block.parseBytes(block.bytes).get

    assert(parsedBlock.consensusDataField.value.asInstanceOf[NxtLikeConsensusBlockData].generationSignature.sameElements(gs))
    assert(parsedBlock.versionField.value == version)
    assert(parsedBlock.signerDataField.value.generator.publicKey.sameElements(gen.publicKey))
  }

  test("Qora block with txs bytes/parse roundtrip") {
    implicit val consensusModule = new QoraLikeConsensusModule()
    implicit val transactionModule = new SimpleTransactionModule()(application.settings, application)

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)

    val gb = Random.nextLong()
    val gs = Array.fill(QoraLikeConsensusModule.GeneratorSignatureLength)(Random.nextInt(100).toByte)

    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
    val cbd = new QoraLikeConsensusBlockData {
      override val generatorSignature: Array[Byte] = gs
      override val generatingBalance: Long = gb
    }

    val cbdBytes = consensusModule.formBlockData(cbd).bytes
    assert(cbdBytes.takeRight(QoraLikeConsensusModule.GeneratorSignatureLength).sameElements(gs))

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    val parsedBlock = Block.parseBytes(block.bytes).get

    val parsedCdf = parsedBlock.consensusDataField.value.asInstanceOf[QoraLikeConsensusBlockData]
    assert(parsedCdf.generatingBalance == gb)
    assert(parsedCdf.generatorSignature.sameElements(gs))
    assert(parsedBlock.versionField.value == version)
    assert(parsedBlock.signerDataField.value.generator.publicKey.sameElements(gen.publicKey))
  }

}