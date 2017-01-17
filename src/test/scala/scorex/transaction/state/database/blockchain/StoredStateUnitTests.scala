package scorex.transaction.state.database.blockchain

import java.io.File
import java.util.UUID

import org.h2.mvstore.MVStore
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.settings.WavesHardForkParameters
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{Order, OrderMatch, OrderType}
import scorex.transaction.state.database.state._
import scorex.utils.{NTP, ScorexLogging}

import scala.util.Random
import scala.util.control.NonFatal

class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
  with PrivateMethodTester with OptionValues with TransactionGen with Assertions with ScorexLogging {

  val forkParametersWithEnableUnissuedAssetsCheck = new AnyRef with WavesHardForkParameters {
    override def allowTemporaryNegativeUntil: Long = 0L

    override def requireSortedTransactionsAfter: Long = Long.MaxValue

    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue

    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MaxValue

    override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MaxValue

    override def allowTransactionsFromFutureUntil: Long = Long.MaxValue

    override def allowUnissuedAssetsUntil: Long = 0L

    override def allowBurnTransactionAfterTimestamp: Long = 0L
  }

  val folder = s"/tmp/scorex/test/${UUID.randomUUID().toString}/"
  new File(folder).mkdirs()
  val stateFile = folder + "state.dat"
  new File(stateFile).delete()

  val db = new MVStore.Builder().fileName(stateFile).compress().open()
  val state = StoredState.fromDB(db, forkParametersWithEnableUnissuedAssetsCheck)
  val testAcc = new PrivateKeyAccount(scorex.utils.randomBytes(64))
  val testAssetAcc = AssetAcc(testAcc, None)
  val testAdd = testAcc.address

  val applyChanges = PrivateMethod[Unit]('applyChanges)
  val calcNewBalances = PrivateMethod[Unit]('calcNewBalances)

  property("Burn assets") {
    forAll(issueReissueGenerator) { pair =>
      withRollbackTest {
        val issueTx: IssueTransaction = pair._1
        val burnTx: BurnTransaction = pair._4
        val senderAddress = issueTx.sender.address
        val senderAmountAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))

        state.assetBalance(senderAmountAcc) shouldBe 0
        state.isValid(issueTx, Int.MaxValue) shouldBe true

        state.applyChanges(state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true))
        state.assetBalance(senderAmountAcc) shouldBe issueTx.quantity

        state.isValid(burnTx, Int.MaxValue) shouldBe true

        state.applyChanges(state.calcNewBalances(Seq(burnTx), Map(), allowTemporaryNegative = true))
        state.assetBalance(senderAmountAcc) shouldBe (issueTx.quantity - burnTx.amount)

        val senderBalances = state.getAccountBalance(issueTx.sender)

        senderBalances.keySet should contain(issueTx.assetId)
        val b = senderBalances.head._2
        b._1 == b._3 shouldBe true
        b._1 == issueTx.quantity - burnTx.amount shouldBe true
      }
    }
  }


  property("Transaction seq Long overflow") {
    val TxN: Int = 12
    val InitialBalance: Long = Long.MaxValue / 8
    state.applyChanges(Map(testAssetAcc -> (AccState(InitialBalance), List(FeesStateChange(InitialBalance)))))
    state.balance(testAcc) shouldBe InitialBalance

    val transfers = (0 until TxN).map { i => genTransfer(InitialBalance - 1, 1) }
    transfers.foreach(tx => state.isValid(tx, tx.timestamp) shouldBe true)

    state.isValid(transfers, blockTime = transfers.map(_.timestamp).max) shouldBe false

    state.applyChanges(Map(testAssetAcc -> (AccState(0L), List())))
  }

  property("Validate transfer with too big amount") {
    val recipient = new PrivateKeyAccount("recipient account".getBytes)

    forAll(positiveLongGen, positiveLongGen) { (balance: Long, fee: Long) =>
      whenever(balance > fee) {
        val assetAcc = AssetAcc(testAcc, None)

        //set some balance
        val genes = GenesisTransaction.create(testAcc, balance, 0).right.get
        state.applyChanges(Map(testAssetAcc -> (AccState(genes.amount), List(genes))))
        state.assetBalance(assetAcc) shouldBe balance

        //valid transfer
        val tx = TransferTransaction.create(None, testAcc, recipient, balance - fee, System.currentTimeMillis(),
          None, fee, Array()).right.get
        state.isValid(tx, tx.timestamp) shouldBe true

        //transfer asset
        state.balance(testAcc) shouldBe balance
        val invalidtx = TransferTransaction.create(None, testAcc, recipient, balance, System.currentTimeMillis(),
          None, fee, Array()).right.get
        state.isValid(invalidtx, invalidtx.timestamp) shouldBe false

        state.applyChanges(Map(testAssetAcc -> (AccState(0L), List(tx))))
      }
    }
  }

  property("Transfer unissued asset to yourself is not allowed") {
    withRollbackTest {
      forAll(selfTransferWithWavesFeeGenerator suchThat (t => t.assetId.isDefined)) { tx: TransferTransaction =>
        val senderAccount = AssetAcc(tx.sender, None)
        val txFee = tx.fee
        state.applyChanges(Map(senderAccount -> (AccState(txFee), List(FeesStateChange(txFee)))))
        state.isValid(Seq(tx), None, System.currentTimeMillis()) shouldBe false
      }
    }
  }

  property("Transfer asset") {
    forAll(transferGenerator) { tx: TransferTransaction =>
      withRollbackTest {
        val senderAmountAcc = AssetAcc(tx.sender, tx.assetId)
        val senderFeeAcc = AssetAcc(tx.sender, tx.feeAssetId)
        val recipientAmountAcc = AssetAcc(tx.recipient, tx.assetId)

        val senderAmountBalance = state.assetBalance(senderAmountAcc)
        val senderFeeBalance = state.assetBalance(senderFeeAcc)
        val recipientAmountBalance = state.assetBalance(recipientAmountAcc)

        state.applyChanges(state.calcNewBalances(Seq(tx), Map(), allowTemporaryNegative = true))

        val newSenderAmountBalance = state.assetBalance(senderAmountAcc)
        val newSenderFeeBalance = state.assetBalance(senderFeeAcc)
        val newRecipientAmountBalance = state.assetBalance(recipientAmountAcc)

        newRecipientAmountBalance shouldBe (recipientAmountBalance + tx.amount)

        val sameAssetForFee = tx.feeAssetId.map(fa => tx.assetId.exists(_ sameElements fa)).getOrElse(tx.assetId.isEmpty)

        if (sameAssetForFee) {
          newSenderAmountBalance shouldBe newSenderFeeBalance
          newSenderAmountBalance shouldBe (senderAmountBalance - tx.amount - tx.fee)
        } else {
          newSenderAmountBalance shouldBe senderAmountBalance - tx.amount
          newSenderFeeBalance shouldBe senderFeeBalance - tx.fee
        }

      }
    }
  }

  property("Transfer asset without balance should fails") {
    withRollbackTest {
      forAll(transferGenerator) { tx: TransferTransaction =>
        val senderAmountAcc = AssetAcc(tx.sender, tx.assetId)
        val senderFeeAcc = AssetAcc(tx.sender, tx.feeAssetId)
        val recipientAmountAcc = AssetAcc(tx.recipient, tx.assetId)

        val senderAmountBalance = state.assetBalance(senderAmountAcc)
        val senderFeeBalance = state.assetBalance(senderFeeAcc)
        val recipientAmountBalance = state.assetBalance(recipientAmountAcc)

        if (tx.amount > 0 && tx.assetId == tx.assetFee._1 && senderAmountBalance < tx.amount + tx.fee) {
          an[Error] should be thrownBy
            state.applyChanges(state.calcNewBalances(Seq(tx), Map(), allowTemporaryNegative = false))
        }
      }
    }
  }

  property("AccountAssetsBalances") {
    forAll(issueGenerator) { tx: IssueTransaction =>
      withRollbackTest {
        state.applyChanges(state.calcNewBalances(Seq(tx), Map(), allowTemporaryNegative = true))

        val recipient = Account.fromPublicKey(tx.sender.publicKey.reverse)
        val transfer = TransferTransaction.create(Some(tx.assetId), tx.sender.asInstanceOf[PrivateKeyAccount],
          recipient, tx.quantity / 2, System.currentTimeMillis(), Some(tx.assetId), tx.quantity / 4,
          Array.emptyByteArray).right.get
        state.applyChanges(state.calcNewBalances(Seq(transfer), Map(), allowTemporaryNegative = true))

        val senderBalances = state.getAccountBalance(tx.sender)
        val receiverBalances = state.getAccountBalance(recipient)

        senderBalances.keySet should contain(tx.assetId)
        receiverBalances.keySet should contain(tx.assetId)
      }
    }
  }

  property("Assets quantity with rollback") {
    forAll(issueGenerator) { tx: IssueTransaction =>
      withRollbackTest {
        state.applyChanges(state.calcNewBalances(Seq(tx), Map(), allowTemporaryNegative = true))
        state.totalAssetQuantity(tx.assetId) shouldBe tx.quantity

        val recipient = Account.fromPublicKey(tx.sender.publicKey.reverse)
        val transfer = TransferTransaction.create(Some(tx.assetId), tx.sender.asInstanceOf[PrivateKeyAccount],
          recipient, tx.quantity / 2, System.currentTimeMillis(), Some(tx.assetId), tx.quantity / 4,
          Array.emptyByteArray).right.get
        state.applyChanges(state.calcNewBalances(Seq(transfer), Map(), allowTemporaryNegative = true))
        state.totalAssetQuantity(tx.assetId) shouldBe tx.quantity

        state.rollbackTo(state.stateHeight - 1)
        state.totalAssetQuantity(tx.assetId) shouldBe tx.quantity
      }
    }
  }

  property("Old style reissue asset") {
    forAll(issueReissueGenerator) { pair =>
      val issueTx: IssueTransaction = pair._1
      val issueTx2: IssueTransaction = pair._2
      val assetAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))

      state.applyChanges(state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true))

      state.isValid(issueTx2, Int.MaxValue) shouldBe false
    }
  }

  property("Reissue asset") {
    forAll(issueReissueGenerator) { pair =>
      withRollbackTest {
        val issueTx: IssueTransaction = pair._1
        val reissueTx: ReissueTransaction = pair._3

        state.isValid(issueTx, Int.MaxValue) shouldBe true

        state.applyChanges(state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true))

        state.isValid(issueTx, Int.MaxValue) shouldBe false

        state.isValid(reissueTx, Int.MaxValue) shouldBe issueTx.reissuable
      }
    }
  }

  property("Issue asset") {
    forAll(issueGenerator) { issueTx: IssueTransaction =>
      withRollbackTest {
        val assetAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))
        val networkAcc = AssetAcc(issueTx.sender, None)

        //set some balance
        val genes = GenesisTransaction.create(issueTx.sender, issueTx.fee + Random.nextInt(1000), issueTx.timestamp - 1).right.get
        state.applyChanges(state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = true))
        state.assetBalance(assetAcc) shouldBe 0
        state.assetBalance(networkAcc) shouldBe genes.amount
        state.balance(issueTx.sender) shouldBe genes.amount

        //issue asset
        state.assetBalance(assetAcc) shouldBe 0
        val newBalances = state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true)
        state.applyChanges(newBalances)
        state.assetBalance(assetAcc) shouldBe issueTx.quantity
        state.assetBalance(networkAcc) shouldBe (genes.amount - issueTx.fee)
      }
    }
  }

  property("accountTransactions returns IssueTransactions") {
    forAll(issueGenerator) { issueTx: IssueTransaction =>
      val assetAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))
      val networkAcc = AssetAcc(issueTx.sender, None)
      //set some balance
      val genes = GenesisTransaction.create(issueTx.sender, issueTx.fee + Random.nextInt(1000), issueTx.timestamp - 1).right.get
      state.applyChanges(state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = true))
      //issue asset
      val newBalances = state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true)
      state.applyChanges(newBalances)
      state.accountTransactions(issueTx.sender).count(_.isInstanceOf[IssueTransaction]) shouldBe 1
    }
  }

  property("accountTransactions returns TransferTransactions if fee in base token") {
    forAll(transferGeneratorWithNoneFeeAssetId) { t: TransferTransaction =>
      val senderAmountAcc = AssetAcc(t.sender, t.assetId)
      val senderFeeAcc = AssetAcc(t.sender, t.feeAssetId)
      val recipientAmountAcc = AssetAcc(t.recipient, t.assetId)
      state.applyChanges(state.calcNewBalances(Seq(t), Map(), allowTemporaryNegative = true))
      state.accountTransactions(t.sender).count(_.isInstanceOf[TransferTransaction]) shouldBe 1
      state.accountTransactions(t.recipient).count(_.isInstanceOf[TransferTransaction]) shouldBe 1
    }
  }

  property("Applying payment transactions") {
    val testAssetAcc = AssetAcc(testAcc, None)
    forAll(paymentGenerator, Gen.posNum[Long]) { (tx: PaymentTransaction,
                                                  balance: Long) =>
      withRollbackTest {
        state.balance(testAcc) shouldBe 0
        state.assetBalance(testAssetAcc) shouldBe 0
        state invokePrivate applyChanges(Map(testAssetAcc -> (AccState(balance), Seq(FeesStateChange(balance), tx, tx))),
          NTP.correctedTime())
        state.balance(testAcc) shouldBe balance
        state.assetBalance(testAssetAcc) shouldBe balance
        state.included(tx).value shouldBe state.stateHeight
        state invokePrivate applyChanges(Map(testAssetAcc -> (AccState(0L), Seq(tx))), NTP.correctedTime())
      }
    }
  }

  property("Validate payment transactions to yourself") {
    forAll(selfPaymentGenerator) { (tx: PaymentTransaction) =>
      withRollbackTest {
        val account = tx.sender
        val assetAccount = AssetAcc(account, None)
        state.balance(account) shouldBe 0
        state.assetBalance(assetAccount) shouldBe 0
        val balance = tx.fee
        state invokePrivate applyChanges(Map(assetAccount -> (AccState(balance), Seq(FeesStateChange(balance)))),
          NTP.correctedTime())
        state.balance(account) shouldBe balance
        state.isValid(tx, System.currentTimeMillis) should be(false)
      }
    }
  }

  property("Validate transfer transactions to yourself") {
    forAll(selfTransferGenerator) { (tx: TransferTransaction) =>
      withRollbackTest {
        val account = tx.sender
        val assetAccount = AssetAcc(account, tx.feeAssetId)
        state.balance(account) shouldBe 0
        state.assetBalance(assetAccount) shouldBe 0
        val balance = tx.fee
        state invokePrivate applyChanges(Map(assetAccount -> (AccState(balance), Seq(FeesStateChange(balance)))),
          NTP.correctedTime())
        state.assetBalance(assetAccount) shouldBe balance
        state.isValid(tx, System.currentTimeMillis) should be(false)
      }
    }
  }

  property("Order matching") {
    forAll { x: (OrderMatch, PrivateKeyAccount) =>
      withRollbackTest {
        def feeInAsset(amount: Long, assetId: Option[AssetId]): Long = {
          if (assetId.isEmpty) amount else 0L
        }

        def amountInWaves(amount: Long, order: Order): Long = {
          if (order.assetPair.first.isEmpty) {
            val sign = if (order.orderType == OrderType.BUY) -1 else 1
            amount * sign
          } else 0L
        }

        val (om, matcher) = x

        val pair = om.buyOrder.assetPair
        val buyer = om.buyOrder.sender
        val seller = om.sellOrder.sender

        val buyerAcc1 = AssetAcc(buyer, pair.first)
        val buyerAcc2 = AssetAcc(buyer, pair.second)
        val sellerAcc1 = AssetAcc(seller, pair.first)
        val sellerAcc2 = AssetAcc(seller, pair.second)
        val buyerFeeAcc = AssetAcc(buyer, None)
        val sellerFeeAcc = AssetAcc(seller, None)
        val matcherFeeAcc = AssetAcc(om.buyOrder.matcher, None)

        val Seq(buyerBal1, buyerBal2, sellerBal1, sellerBal2, buyerFeeBal, sellerFeeBal, matcherFeeBal) =
          getBalances(buyerAcc1, buyerAcc2, sellerAcc1, sellerAcc2, buyerFeeAcc, sellerFeeAcc, matcherFeeAcc)

        state.applyChanges(state.calcNewBalances(Seq(om), Map(), allowTemporaryNegative = true))

        val Seq(newBuyerBal1, newBuyerBal2, newSellerBal1, newSellerBal2, newBuyerFeeBal, newSellerFeeBal, newMatcherFeeBal) =
          getBalances(buyerAcc1, buyerAcc2, sellerAcc1, sellerAcc2, buyerFeeAcc, sellerFeeAcc, matcherFeeAcc)

        newBuyerBal1 should be(buyerBal1 - om.amount - feeInAsset(om.buyMatcherFee, buyerAcc1.assetId))
        newBuyerBal2 should be(buyerBal2 + BigInt(om.amount) * Order.PriceConstant / om.price -
          feeInAsset(om.buyMatcherFee, buyerAcc2.assetId))
        newSellerBal1 should be(sellerBal1 + om.amount - feeInAsset(om.sellMatcherFee, sellerAcc1.assetId))
        newSellerBal2 should be(sellerBal2 - BigInt(om.amount) * Order.PriceConstant / om.price -
          feeInAsset(om.sellMatcherFee, sellerAcc2.assetId))
        newBuyerFeeBal should be(buyerFeeBal - om.buyMatcherFee + amountInWaves(om.amount, om.buyOrder))
        newSellerFeeBal should be(sellerFeeBal - om.sellMatcherFee + amountInWaves(om.amount, om.sellOrder))
        newMatcherFeeBal should be(matcherFeeBal + om.buyMatcherFee + om.sellMatcherFee - om.fee)
      }
    }
  }

  property("Reopen state") {
    val balance = 1234L
    state invokePrivate applyChanges(Map(testAssetAcc -> (AccState(balance), Seq(FeesStateChange(balance)))),
      NTP.correctedTime())
    state.balance(testAcc) shouldBe balance
    db.close()
  }

  private var txTime: Long = 0

  private def getTimestamp: Long = synchronized {
    txTime = Math.max(System.currentTimeMillis(), txTime + 1)
    txTime
  }

  def genTransfer(amount: Long, fee: Long): TransferTransaction = {
    val recipient = new PrivateKeyAccount(scorex.utils.randomBytes())
    TransferTransaction.create(None, testAcc, recipient: Account, amount, getTimestamp, None, fee, Array()).right.get
  }

  def genPayment(amount: Long, fee: Long): PaymentTransaction = {
    val recipient = new PrivateKeyAccount(scorex.utils.randomBytes())
    val time = getTimestamp
    PaymentTransaction.create(testAcc, recipient, amount, fee, time).right.get
  }

  private def withRollbackTest(test: => Unit): Unit = {
    val startedState = state.stateHeight
    val h = state.hash
    var lastFinalStateHash: Option[Int] = None
    for {i <- 1 to 2} {
      try {
        test
        state.rollbackTo(startedState)
        h should be(state.hash)
        lastFinalStateHash match {
          case Some(lastHash) =>
            lastHash should be(state.hash)
          case None =>
            lastFinalStateHash = Some(state.hash)
        }
      } catch {
        case NonFatal(e) =>
          log.error(s"Failed on $i iteration: ${e.getMessage}")
          throw e
      }
    }
  }

  def getBalances(a: AssetAcc*): Seq[Long] = {
    a.map(state.assetBalance(_))
  }

}
