package scorex.lagonaki.unit

import com.wavesplatform.state2._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class MicroBlockSpecification extends FunSuite with Matchers with MockFactory {

  test("MicroBlock with txs bytes/parse roundtrip") {

    val prevResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
    val totalResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = PrivateKeyAccount(reference)

    val ts = System.currentTimeMillis() - 5000
    val sender = PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction.create(sender, gen, 5, 1000, ts).right.get
    val tr: TransferTransaction = TransferTransaction.create(None, sender, gen, 5, ts + 1, None, 2, Array()).right.get
    val assetId = Some(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction = TransferTransaction.create(assetId, sender, gen, 5, ts + 2, None, 2, Array()).right.get

    val transactions = Seq(tx, tr, tr2)

    val version = 8: Byte
    val timestamp = System.currentTimeMillis()

    val microBlock = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig).explicitGet()
    val parsedBlock = MicroBlock.parseBytes(microBlock.bytes).get

    assert(Signed.validateSignatures(microBlock).isRight)
    assert(Signed.validateSignatures(parsedBlock).isRight)

    assert(microBlock.signature == parsedBlock.signature)
    assert(microBlock.generator == parsedBlock.generator)
    assert(microBlock.totalResBlockSig == parsedBlock.totalResBlockSig)
    assert(microBlock.prevResBlockSig == parsedBlock.prevResBlockSig)
    assert(microBlock.transactionData == parsedBlock.transactionData)
    assert(microBlock == parsedBlock)
  }

}
