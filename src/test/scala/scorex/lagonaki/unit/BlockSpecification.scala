package scorex.lagonaki.unit

import com.wavesplatform.BlockGen
import com.wavesplatform.state2.{ByteStr, Instrumented}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class BlockSpecification extends FunSuite with Matchers with MockFactory with BlockGen with GeneratorDrivenPropertyChecks {

  test(" block with txs bytes/parse roundtrip") {

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = PrivateKeyAccount(reference)

    val bt = Random.nextLong()
    val gs = ByteStr(Array.fill(Block.GeneratorSignatureLength)(Random.nextInt(100).toByte))


    val ts = System.currentTimeMillis() - 5000
    val sender = PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction.create(sender, gen, 5, 1000, ts).right.get
    val tr: TransferTransaction = TransferTransaction.create(None, sender, gen, 5, ts + 1, None, 2, Array()).right.get
    val assetId = Some(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction = TransferTransaction.create(assetId, sender, gen, 5, ts + 2, None, 2, Array()).right.get

    val tbd = Seq(tx, tr, tr2)
    val cbd = NxtLikeConsensusBlockData(bt, gs)

    def testBlock(txs: Seq[Transaction])(version: Int) = {
      val timestamp = System.currentTimeMillis()
      val block = Block.buildAndSign(version.toByte, timestamp, ByteStr(reference), cbd, txs, gen)
      val parsedBlock = Block.parseBytes(block.bytes).get
      assert(Signed.validateSignatures(block).isRight)
      assert(Signed.validateSignatures(parsedBlock).isRight)
      assert(parsedBlock.consensusData.generationSignature == gs)
      assert(parsedBlock.version.toInt == version)
      assert(parsedBlock.signerData.generator.publicKey.sameElements(gen.publicKey))
    }

    List(1, 2).foreach(testBlock(tbd))
    Range(40, 80).foreach(x => testBlock(Seq.fill(x)(tbd).flatten)(3))
  }

  ignore ("sign time for 60k txs") {
    forAll(randomTransactionsGen(60000), accountGen, byteArrayGen(Block.BlockIdLength), byteArrayGen(Block.GeneratorSignatureLength)) { case ((txs, acc, ref, gs)) =>
      val (block, t0) = Instrumented.withTime(Block.buildAndSign(3, 1, ByteStr(ref), NxtLikeConsensusBlockData(1, ByteStr(gs)), txs, acc))
      val (bytes, t1) = Instrumented.withTime(block.bytesWithoutSignature)
      val (hash, t2) = Instrumented.withTime(FastCryptographicHash.hash(bytes))
      val (sig, t3) = Instrumented.withTime(EllipticCurveImpl.sign(acc, hash))
      println((t0, t1, t2,t3))
    }

  }
}
