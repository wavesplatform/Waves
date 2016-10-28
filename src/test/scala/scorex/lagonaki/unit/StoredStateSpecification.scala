package scorex.lagonaki.unit

import java.io.File
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random
import org.h2.mvstore.MVStore
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import scorex.account.{Account, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.BlockMock
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.wallet.{IssueRequest, TransferRequest}
import scorex.transaction.{AssetAcc, GenesisTransaction}
import scorex.wallet.Wallet

class StoredStateSpecification extends FunSuite with Matchers with TableDrivenPropertyChecks {

  val folder = "/tmp/scorex/test/"
  new File(folder).mkdirs()
  val stateFile = folder + "state.dat"
  new File(stateFile).delete()

  val wallet = new Wallet(None, "123", Some(Array(0.toByte, 1.toByte)))
  val accounts = wallet.generateNewAccounts(3)

  val db = new MVStore.Builder().fileName(stateFile).compress().open()
  val state = new StoredState(db)
  state.processBlock(new BlockMock(Seq(GenesisTransaction(accounts.head, 100000000000L, 0))))

  private def createIssueAssetTx(request: IssueRequest, wallet: Wallet): IssueTransaction = {
    val sender = wallet.privateKeyAccount(request.sender).get
    IssueTransaction.create(sender,
      None,
      Base58.decode(request.name).get,
      Base58.decode(request.description).get,
      request.quantity,
      request.decimals,
      request.reissuable,
      request.fee,
      i.incrementAndGet())
  }

  private val i = new AtomicInteger
  private def createTransferAssetTx(request: TransferRequest, wallet: Wallet): TransferTransaction = {
    val sender = wallet.privateKeyAccount(request.sender).get
    TransferTransaction.create(request.assetIdOpt.map(s => Base58.decode(s).get),
      sender: PrivateKeyAccount,
      new Account(request.recipient),
      request.amount,
      i.incrementAndGet(),
      request.feeAsset.map(s => Base58.decode(s).get),
      request.feeAmount,
      Base58.decode(request.attachment).get)
  }

  test("many transfer asset transactions") {
    val acc = accounts.head
    val startWavesBalance = state.balance(acc)

    val recipients = Seq(
      new PrivateKeyAccount(Array(34.toByte, 1.toByte)),
      new PrivateKeyAccount(Array(1.toByte, 23.toByte))
    )

    val issueAssetTx = createIssueAssetTx(IssueRequest(acc.address, "AAAAB", "BBBBB", 1000000, 2, reissuable = false,
      100000000), wallet)
    state.processBlock(new BlockMock(Seq(issueAssetTx))) should be('success)
    val assetId = Some(Base58.encode(issueAssetTx.assetId))

    val txs = recipients.flatMap(r => Seq.fill(10) {
      createTransferAssetTx(TransferRequest(assetId, None, 10, 1, acc.address, "123", r.address), wallet)
    })

    state.processBlock(new BlockMock(Random.shuffle(txs))) should be('success)

    recipients.foreach(r => state.assetBalance(AssetAcc(r, Some(issueAssetTx.assetId))) should be(100))

    state.assetBalance(AssetAcc(acc, Some(issueAssetTx.assetId))) should be(999800)
    state.balance(acc) should be(startWavesBalance - 100000000 - 20)
}

  test("many transfer waves transactions") {
    val acc = accounts.head
    val startWavesBalance = state.balance(acc)

    val recipients = Seq(
      new PrivateKeyAccount(Array(37.toByte, 1.toByte)),
      new PrivateKeyAccount(Array(8.toByte, 23.toByte))
    )

    val txs = recipients.flatMap(r => Seq.fill(10) {
      Thread.sleep(1)
      createTransferAssetTx(TransferRequest(None, None, 10, 1, acc.address, "123", r.address), wallet)
    })

    state.processBlock(new BlockMock(Random.shuffle(txs))) should be('success)

    recipients.foreach(r => state.assetBalance(AssetAcc(r, None)) should be(100))

    state.balance(acc) should be(startWavesBalance - 200 - 20)
  }

  test("issues and many transfer assets with transactions with fee") {
    val sender = accounts.head
    val feeGetter = accounts.last
    val blahBlahBase58 = Base58.encode("1234567890".getBytes)

    val wavesTransferForAssets = createIssueAssetTx(IssueRequest(sender.address, blahBlahBase58,
      blahBlahBase58, 20000, 0, reissuable = false, 1000000000L), wallet)

    val assetId = wavesTransferForAssets.id
    val assetIdString = Base58.encode(wavesTransferForAssets.id)
    state.processBlock(new BlockMock(Seq(wavesTransferForAssets), new PublicKeyAccount(feeGetter.publicKey))) should be(
      'success)

    val txs = Seq.fill(10) {
      createTransferAssetTx(TransferRequest(Some(assetIdString), Some(assetIdString),
        1000, 1000, sender.address, blahBlahBase58, sender.address), wallet)
    }
    state.processBlock(new BlockMock(txs, new PublicKeyAccount(feeGetter.publicKey))) should be('success)

    val senderAssetBalance = state.assetBalance(AssetAcc(new Account(sender.address), Some(assetId)))
    senderAssetBalance shouldBe 10000
    val feeGetterAssetBalace = state.assetBalance(AssetAcc(new PublicKeyAccount(accounts.last.publicKey), Some(
      assetId)))
    feeGetterAssetBalace shouldBe 10000
  }

  test("issues and many transfer assets with transactions with fee and without") {
    val sender = accounts.head
    val feeGetter = accounts.last
    val blahBlahBase58 = Base58.encode("1234567890".getBytes)

    val wavesTransferForAssets = createIssueAssetTx(IssueRequest(sender.address, blahBlahBase58,
      blahBlahBase58, 20000, 0, reissuable = false, 1000000000L), wallet)

    val assetId = wavesTransferForAssets.id
    val assetIdString = Base58.encode(wavesTransferForAssets.id)
    state.processBlock(new BlockMock(Seq(wavesTransferForAssets), new PublicKeyAccount(feeGetter.publicKey))) should be(
      'success)

    val txs = Seq.fill(10) {
      createTransferAssetTx(TransferRequest(Some(assetIdString), Some(assetIdString),
        1000, 1000, sender.address, blahBlahBase58, sender.address), wallet)
    }
    val txs2 = Seq.fill(10) {
      createTransferAssetTx(TransferRequest(Some(assetIdString), None,
        1000, 1000, sender.address, blahBlahBase58, sender.address), wallet)
    }
    state.processBlock(new BlockMock(txs ++ txs2, new PublicKeyAccount(feeGetter.publicKey))) should be('success)

    val senderAssetBalance = state.assetBalance(AssetAcc(new Account(sender.address), Some(assetId)))
    senderAssetBalance shouldBe 10000
    val feeGetterAssetBalace = state.assetBalance(AssetAcc(new PublicKeyAccount(accounts.last.publicKey), Some(
      assetId)))
    feeGetterAssetBalace shouldBe 10000
  }
}
