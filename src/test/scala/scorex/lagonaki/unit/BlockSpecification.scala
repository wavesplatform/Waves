package scorex.lagonaki.unit

import com.wavesplatform.state2._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class BlockSpecification extends FunSuite with Matchers with MockFactory {

  val reference: Array[Byte] = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  val gen = PrivateKeyAccount(reference)
  val bt: Long = Random.nextLong()
  val gs: Array[Byte] = Array.fill(Block.GeneratorSignatureLength)(Random.nextInt(100).toByte)
  val ts: Long = System.currentTimeMillis() - 5000
  val sender = PrivateKeyAccount(reference.dropRight(2))
  val tx: Transaction = PaymentTransaction.create(sender, gen, 5, 1000, ts).right.get
  val tr: TransferTransaction = TransferTransaction.create(None, sender, gen, 5, ts + 1, None, 2, Array()).right.get
  val assetId = Some(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
  val tr2: TransferTransaction = TransferTransaction.create(assetId, sender, gen, 5, ts + 2, None, 2, Array()).right.get
  val tbd = Seq(tx, tr, tr2)
  val cbd = NxtLikeConsensusBlockData(bt, gs)

  test(" block with txs bytes/parse roundtrip version 3") {

    val timestamp = System.currentTimeMillis()
    val version = 3
    val supportedFeatures = Array[Short](123, 124, 135)

    val block = Block.buildAndSign(version.toByte, timestamp, ByteStr(reference), cbd, tbd, gen, supportedFeatures).explicitGet()
    val parsedBlock = Block.parseBytes(block.bytes).get
    assert(Signed.validateSignatures(block).isRight)
    assert(Signed.validateSignatures(parsedBlock).isRight)
    assert(parsedBlock.consensusData.generationSignature.sameElements(gs))
    assert(parsedBlock.version.toInt == version)
    parsedBlock.supportedFeaturesIds shouldEqual supportedFeatures
    assert(parsedBlock.signerData.generator.publicKey.sameElements(gen.publicKey))
  }

  test(" block with txs bytes/parse roundtrip version 1,2") {

    val timestamp = System.currentTimeMillis()

    List(1, 2).foreach { version =>

      val block = Block.buildAndSign(version.toByte, timestamp, ByteStr(reference), cbd, tbd, gen).explicitGet()
      val parsedBlock = Block.parseBytes(block.bytes).get
      assert(Signed.validateSignatures(block).isRight)
      assert(Signed.validateSignatures(parsedBlock).isRight)
      assert(parsedBlock.consensusData.generationSignature.sameElements(gs))
      assert(parsedBlock.version.toInt == version)
      assert(parsedBlock.signerData.generator.publicKey.sameElements(gen.publicKey))
    }
  }
}
