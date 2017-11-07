package scorex.lagonaki.unit

import com.wavesplatform.mining.Miner
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.produce
import org.scalamock.scalatest.MockFactory
import org.scalatest.words.ShouldVerb
import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class MicroBlockSpecification extends FunSuite with Matchers with MockFactory with ShouldVerb{

  val prevResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  val totalResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  val sender = PrivateKeyAccount(reference.dropRight(2))
  val gen = PrivateKeyAccount(reference)

  test("MicroBlock with txs bytes/parse roundtrip") {

    val ts = System.currentTimeMillis() - 5000
    val tx: Transaction = PaymentTransaction.create(sender, gen, 5, 1000, ts).right.get
    val tr: TransferTransaction = TransferTransaction.create(None, sender, gen, 5, ts + 1, None, 2, Array()).right.get
    val assetId = Some(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction = TransferTransaction.create(assetId, sender, gen, 5, ts + 2, None, 2, Array()).right.get

    val transactions = Seq(tx, tr, tr2)

    val microBlock = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig).explicitGet()
    val parsedBlock = MicroBlock.parseBytes(microBlock.bytes()).get

    assert(microBlock.signaturesValid().isRight)
    assert(parsedBlock.signaturesValid().isRight)

    assert(microBlock.signature == parsedBlock.signature)
    assert(microBlock.generator == parsedBlock.generator)
    assert(microBlock.totalResBlockSig == parsedBlock.totalResBlockSig)
    assert(microBlock.prevResBlockSig == parsedBlock.prevResBlockSig)
    assert(microBlock.transactionData == parsedBlock.transactionData)
    assert(microBlock == parsedBlock)
  }

  test("MicroBlock cannot be created with zero transactions") {

    val transactions = Seq.empty[PaymentTransaction]
    val eitherBlockOrError = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig)

    eitherBlockOrError should produce("cannot create empty MicroBlock")
  }

  test("MicroBlock cannot contain more than Miner.MaxTransactionsPerMicroblock") {

    val transaction = PaymentTransaction.create(sender, gen, 5, 1000, System.currentTimeMillis()).right.get
    val transactions = Seq.fill(Miner.MaxTransactionsPerMicroblock + 1)(transaction)

    val eitherBlockOrError = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig)

    eitherBlockOrError should produce("too many txs in MicroBlock")
  }
}
