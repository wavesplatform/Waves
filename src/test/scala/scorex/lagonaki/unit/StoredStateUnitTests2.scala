package scorex.lagonaki.unit

import java.util.concurrent.atomic.AtomicInteger

import org.h2.mvstore.MVStore
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http.assets.{IssueRequest, TransferRequest}
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestBlockchainSettings
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{AssetAcc, GenesisTransaction}
import scorex.wallet.Wallet

import scala.util.Random

class StoredStateUnitTests2 extends FunSuite with Matchers with TableDrivenPropertyChecks {

  private val stateFile = scorex.createTestTemporaryFile("state", ".dat")

  val wallet = new Wallet(None, "123", Some(Array(0.toByte, 1.toByte)))
  val accounts = wallet.generateNewAccounts(3)

  val db = new MVStore.Builder().fileName(stateFile.getAbsolutePath).compress().open()
  val state = StoredState.fromDB(db, TestBlockchainSettings.Disabled.functionalitySettings)
  state.processBlock(TestBlock(Seq(GenesisTransaction.create(accounts.head, 100000000000L, 0).right.get)))

  private def createIssueAssetTx(request: IssueRequest, wallet: Wallet): IssueTransaction = {
    val sender = wallet.findWallet(request.sender).right.get
    IssueTransaction.create(sender,
      Base58.decode(request.name).get,
      Base58.decode(request.description).get,
      request.quantity,
      request.decimals,
      request.reissuable,
      request.fee,
      i.incrementAndGet()).right.get
  }

  private val i = new AtomicInteger

  private def createTransferAssetTx(request: TransferRequest, wallet: Wallet): TransferTransaction = {
    val sender = wallet.findWallet(request.sender).right.get
    TransferTransaction.create(request.assetId.map(s => Base58.decode(s).get),
      sender: PrivateKeyAccount,
      Account.fromString(request.recipient).right.get,
      request.amount,
      i.incrementAndGet(),
      request.feeAssetId.map(s => Base58.decode(s).get),
      request.fee,
      if (request.attachment.nonEmpty) {
        request.attachment.flatMap(Base58.decode(_).toOption).get
      } else {
        Array.empty
      }).right.get
  }

  test("many transfer asset transactions") {
    val acc = accounts.head
    val startWavesBalance = state.balance(acc)

    val recipients = Seq(
      PrivateKeyAccount(Array(34.toByte, 1.toByte)),
      PrivateKeyAccount(Array(1.toByte, 23.toByte))
    )

    val issueAssetTx = createIssueAssetTx(IssueRequest(acc.address, "AAAAB", "BBBBB", 1000000, 2, reissuable = false,
      100000000), wallet)
    state.processBlock(TestBlock(Seq(issueAssetTx))) should be('success)
    val assetId = Some(Base58.encode(issueAssetTx.assetId))

    val txs = recipients.flatMap(r => Seq.fill(10) {
      createTransferAssetTx(TransferRequest(assetId, None, 10, 1, acc.address, Some("123"), r.address), wallet)
    })

    state.processBlock(TestBlock(Random.shuffle(txs))) should be('success)

    recipients.foreach(r => state.assetBalance(AssetAcc(r, Some(issueAssetTx.assetId))) should be(100))

    state.assetBalance(AssetAcc(acc, Some(issueAssetTx.assetId))) should be(999800)
    state.balance(acc) should be(startWavesBalance - 100000000 - 20)
  }

  test("many transfer waves transactions") {
    val acc = accounts.head
    val startWavesBalance = state.balance(acc)

    val recipients = Seq(
      PrivateKeyAccount(Array(37.toByte, 1.toByte)),
      PrivateKeyAccount(Array(8.toByte, 23.toByte))
    )

    val txs = recipients.flatMap(r => Seq.fill(10) {
      Thread.sleep(1)
      createTransferAssetTx(TransferRequest(None, None, 10, 1, acc.address, Some("123"), r.address), wallet)
    })

    state.processBlock(TestBlock(Random.shuffle(txs))) should be('success)

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
    state.processBlock(TestBlock(Seq(wavesTransferForAssets), PublicKeyAccount(feeGetter.publicKey))) should be(
      'success)

    val txs = Seq.fill(10) {
      createTransferAssetTx(TransferRequest(Some(assetIdString), Some(assetIdString),
        1000, 1000, sender.address, Some(blahBlahBase58), sender.address), wallet)
    }
    state.processBlock(TestBlock(txs, PublicKeyAccount(feeGetter.publicKey))) should be('success)

    val senderAssetBalance = state.assetBalance(AssetAcc(Account.fromString(sender.address).right.get, Some(assetId)))
    senderAssetBalance shouldBe 10000
    val feeGetterAssetBalace = state.assetBalance(AssetAcc(PublicKeyAccount(accounts.last.publicKey), Some(
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
    state.processBlock(TestBlock(Seq(wavesTransferForAssets), PublicKeyAccount(feeGetter.publicKey))) should be(
      'success)

    val txs = Seq.fill(10) {
      createTransferAssetTx(TransferRequest(Some(assetIdString), Some(assetIdString),
        1000, 1000, sender.address, Some(blahBlahBase58), sender.address), wallet)
    }
    val txs2 = Seq.fill(10) {
      createTransferAssetTx(TransferRequest(Some(assetIdString), None,
        1000, 1000, sender.address, Some(blahBlahBase58), sender.address), wallet)
    }
    state.processBlock(TestBlock(txs ++ txs2, PublicKeyAccount(feeGetter.publicKey))) should be('success)

    val senderAssetBalance = state.assetBalance(AssetAcc(Account.fromString(sender.address).right.get, Some(assetId)))
    senderAssetBalance shouldBe 10000
    val feeGetterAssetBalace = state.assetBalance(AssetAcc(PublicKeyAccount(accounts.last.publicKey), Some(
      assetId)))
    feeGetterAssetBalace shouldBe 10000
  }
}
