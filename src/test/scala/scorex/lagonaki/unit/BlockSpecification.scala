package scorex.lagonaki.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.settings.TestBlockchainSettings
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.duration._
import scala.util.Random

class BlockSpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {

  test("Nxt block with txs bytes/parse roundtrip") {
    implicit val consensusModule = new WavesConsensusModule(TestBlockchainSettings.Disabled)
    implicit val transactionModule = new SimpleTransactionModule(TestBlockchainSettings.Disabled.genesisSettings)(application.settings, application)

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = PrivateKeyAccount(reference)

    val bt = Random.nextLong()
    val gs = Array.fill(WavesConsensusModule.GeneratorSignatureLength)(Random.nextInt(100).toByte)


    val ts = System.currentTimeMillis() - 5000
    val sender = PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction.create(sender, gen, 5, 1000, ts).right.get
    val tr: TransferTransaction = TransferTransaction.create(None, sender, gen, 5, ts + 1, None, 2, Array()).right.get
    val assetId = Some(Array.fill(AssetIdLength)(Random.nextInt(100).toByte))
    val tr2: TransferTransaction = TransferTransaction.create(assetId, sender, gen, 5, ts + 2, None, 2, Array()).right.get

    val tbd = Seq(tx, tr, tr2)
    val cbd = NxtLikeConsensusBlockData(bt, gs)

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    val parsedBlock = Block.parseBytes(block.bytes).get

    assert(parsedBlock.consensusDataField.value.asInstanceOf[NxtLikeConsensusBlockData].generationSignature.sameElements(gs))
    assert(parsedBlock.versionField.value == version)
    assert(parsedBlock.signerDataField.value.generator.publicKey.sameElements(gen.publicKey))
  }

}
