package scorex.transaction.state.database.blockchain

import java.io.File
import java.util.UUID
import scala.util.Random
import scala.util.control.NonFatal
import org.h2.mvstore.MVStore
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.{Account, AddressScheme, PrivateKeyAccount}
import scorex.settings.{ChainParameters, TestChainParameters}
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state._
import scorex.utils.{NTP, ScorexLogging}
import scorex.waves.TestingCommons

class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
  with PrivateMethodTester with OptionValues with TransactionGen with Assertions with ScorexLogging with TestingCommons {

  val forkParametersWithEnableUnissuedAssetsAndLeasingTxCheck = new ChainParameters with TestChainParameters.GenesisData {
    override def allowTemporaryNegativeUntil: Long = 0L

    override def requireSortedTransactionsAfter: Long = Long.MaxValue

    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue

    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MaxValue

    override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MaxValue

    override def allowTransactionsFromFutureUntil: Long = Long.MaxValue

    override def allowLeaseTransactionAfterTimestamp: Long = Long.MinValue

    override def allowUnissuedAssetsUntil: Long = 0L

    override def allowBurnTransactionAfterTimestamp: Long = 0L

    override def requirePaymentUniqueId: Long = 0L

    override def initialBalance: Long = ???

    override def genesisTimestamp: Long = ???

    override def addressScheme: AddressScheme = ???
  }

  val folder = s"/tmp/scorex/test/${UUID.randomUUID().toString}/"
  new File(folder).mkdirs()
  val stateFile = folder + "state.dat"
  new File(stateFile).delete()

  val db = new MVStore.Builder().fileName(stateFile).compress().open()
  val state = StoredState.fromDB(db, forkParametersWithEnableUnissuedAssetsAndLeasingTxCheck)
  val testAcc = new PrivateKeyAccount(scorex.utils.randomBytes(64))
  val testAssetAcc = AssetAcc(testAcc, None)
  val testAdd = testAcc.address

  val applyChanges = PrivateMethod[Unit]('applyChanges)
  val calcNewBalances = PrivateMethod[Unit]('calcNewBalances)


  property("validate plenty of transactions") {
    val TxN: Int = 1000
    val InitialBalance: Long = Long.MaxValue / 8
    state.applyChanges(Map(testAssetAcc -> (AccState(InitialBalance, InitialBalance), List(FeesStateChange(InitialBalance)))))
    state.balance(testAcc) shouldBe InitialBalance
    val trans = (0 until TxN).map { i => genTransfer(InitialBalance - 1, 1) }

    val bts = trans.map(_.timestamp).max
    val (time, result) = profile(state.validate(trans, blockTime = bts))
    time should be < 1000L
    result.size should be <= trans.size
  }

  property("Burn assets") {
    forAll(issueReissueGenerator) { pair =>
      withRollbackTest {
        val issueTx: IssueTransaction = pair._1
        val burnTx: BurnTransaction = pair._4
        val senderAddress = issueTx.sender.address
        val senderAmountAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))

        state.assetBalance(senderAmountAcc) shouldBe 0
        state.validateAgainstState(issueTx, Int.MaxValue) shouldBe an[Right[_, _]]

        state.applyChanges(state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true))
        state.assetBalance(senderAmountAcc) shouldBe issueTx.quantity

        state.validateAgainstState(burnTx, Int.MaxValue) shouldBe an[Right[_, _]]

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
    state.applyChanges(Map(testAssetAcc -> (AccState(InitialBalance, InitialBalance), List(FeesStateChange(InitialBalance)))))
    state.balance(testAcc) shouldBe InitialBalance
    state.effectiveBalance(testAcc) shouldBe InitialBalance

    val transfers = (0 until TxN).map { i => genTransfer(InitialBalance - 1, 1) }
    val invalidTxs = transfers.filterNot(tx => state.isValid(tx, tx.timestamp))
    invalidTxs shouldBe 'isEmpty

    state.isValid(transfers, blockTime = transfers.map(_.timestamp).max) shouldBe false

    state.applyChanges(Map(testAssetAcc -> (AccState(0L, 0L), List())))
  }

  property("Validate transfer with too big amount") {
    val recipient = new PrivateKeyAccount("recipient account".getBytes)

    forAll(positiveLongGen, positiveLongGen) { (balance: Long, fee: Long) =>
      whenever(balance > fee) {
        val assetAcc = AssetAcc(testAcc, None)

        //set some balance
        val genes = GenesisTransaction.create(testAcc, balance, 0).right.get
        state.applyChanges(Map(testAssetAcc -> (AccState(genes.amount, genes.amount), List(genes))))
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

        state.applyChanges(Map(testAssetAcc -> (AccState(0L, 0L), List(tx))))
      }
    }
  }

  property("Transfer unissued asset to yourself is not allowed") {
    withRollbackTest {
      forAll(selfTransferWithWavesFeeGenerator suchThat (t => t.assetId.isDefined)) { tx: TransferTransaction =>
        val senderAccount = AssetAcc(tx.sender, None)
        val txFee = tx.fee
        state.applyChanges(Map(senderAccount -> (AccState(txFee, txFee), List(FeesStateChange(txFee)))))
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

  private def senderAndRecipientStateBalances(tx: LeaseTransaction): (Long, Long, Long, Long) = {
    import tx.sender
    import tx.recipient

    val senderBalance = state.balance(sender)
    val senderEffectiveBalance = state.effectiveBalance(sender)
    val recipientBalance = state.balance(recipient)
    val recipientEffectiveBalance = state.effectiveBalance(recipient)
    (senderBalance, senderEffectiveBalance, recipientBalance, recipientEffectiveBalance)
  }

  private def stateBalances(a: Account): (Long, Long) = {
    val senderBalance = state.balance(a)
    val senderEffectiveBalance = state.effectiveBalance(a)
    (senderBalance, senderEffectiveBalance)
  }

  property("Lease transaction") {
    forAll(leaseGenerator) { tx: LeaseTransaction =>
      withRollbackTest {

        //set some balance
        val balance = tx.amount + tx.fee
        val genes = GenesisTransaction.create(tx.sender, balance, tx.timestamp - 1).right.get
        val genesisBalances = state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = false)
        state.applyChanges(genesisBalances)

        state.balance(tx.sender) shouldBe balance
        state.effectiveBalance(tx.sender) shouldBe balance

        val (senderBalance, senderEffectiveBalance, recipientBalance, recipientEffectiveBalance) = senderAndRecipientStateBalances(tx)

        state.leaseExtendedState.storage.getLeasedSum(tx.sender.address) shouldBe 0L
        state.isValid(tx, Int.MaxValue) shouldBe true

        // apply lease tx
        state.applyChanges(state.calcNewBalances(Seq(tx), Map(), allowTemporaryNegative = false))

        state.isValid(tx, Int.MaxValue) shouldBe false

        val (newSenderBalance, newSenderEffectiveBalance, newRecipientBalance, newRecipientEffectiveBalance) = senderAndRecipientStateBalances(tx)

        newSenderBalance shouldBe (senderBalance - tx.fee)
        newRecipientBalance shouldBe recipientBalance

        newSenderEffectiveBalance shouldBe (senderEffectiveBalance - tx.amount - tx.fee)
        newRecipientEffectiveBalance shouldBe (recipientEffectiveBalance + tx.amount)
      }
    }
  }

  property("Lease transaction without your own effective balance") {
    forAll(twoLeasesGenerator suchThat (p => p._2.amount + p._2.fee < p._1.amount + p._1.fee)) { case (first: LeaseTransaction, second: LeaseTransaction) =>
      withRollbackTest {
        //set some balance
        val balance = first.amount + first.fee
        val genes = GenesisTransaction.create(first.sender, balance, first.timestamp - 1).right.get
        val genesisBalances = state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = false)
        state.applyChanges(genesisBalances)

        state.balance(first.sender) shouldBe balance
        state.effectiveBalance(first.sender) shouldBe balance

        state.leaseExtendedState.storage.getLeasedSum(first.sender.address) shouldBe 0L

        state.isValid(first, Int.MaxValue) shouldBe true
        state.isValid(second, Int.MaxValue) shouldBe true

        // apply lease tx
        state.applyChanges(state.calcNewBalances(Seq(first), Map(), allowTemporaryNegative = false))

        state.isValid(first, Int.MaxValue) shouldBe false
        state.isValid(second, Int.MaxValue) shouldBe false
      }
    }
  }

  property("Lease transaction without fee") {
    forAll(leaseGenerator) { tx: LeaseTransaction =>
      withRollbackTest {


        //set some balance
        val balance = tx.amount
        val genes = GenesisTransaction.create(tx.sender, balance, tx.timestamp - 1).right.get
        state.applyChanges(state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = false))

        state.balance(tx.sender) shouldBe balance
        state.effectiveBalance(tx.sender) shouldBe balance
        balance < tx.amount + tx.fee shouldBe true

        state.isValid(tx, Int.MaxValue) shouldBe false
      }
    }
  }

  property("Lease transaction without amount") {
    forAll(leaseGenerator) { tx: LeaseTransaction =>
      withRollbackTest {

        //set some balance
        val balance = tx.fee
        val genes = GenesisTransaction.create(tx.sender, balance, tx.timestamp - 1).right.get
        state.applyChanges(state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = false))

        state.balance(tx.sender) shouldBe balance
        state.effectiveBalance(tx.sender) shouldBe balance
        balance < tx.amount + tx.fee shouldBe true

        state.isValid(tx, Int.MaxValue) shouldBe false
      }
    }
  }

  property("Lease cancel transaction") {
    forAll(leaseAndCancelGenerator) { case (lease: LeaseTransaction, cancel: LeaseCancelTransaction) =>
      withRollbackTest {
        val balance = lease.amount + lease.fee + cancel.fee

        //set some balance
        val genes = GenesisTransaction.create(lease.sender, balance, lease.timestamp - 1).right.get
        state.applyChanges(state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = false))

        state.balance(lease.sender) shouldBe balance
        state.effectiveBalance(lease.sender) shouldBe balance

        val (senderBalance, senderEffectiveBalance, recipientBalance, recipientEffectiveBalance) = senderAndRecipientStateBalances(lease)

        state.isValid(cancel, Int.MaxValue) shouldBe false

        state.leaseExtendedState.storage.getLeasedSum(lease.sender.address) shouldBe 0L
        state.isValid(lease, Int.MaxValue) shouldBe true

        // apply lease tx
        val balancesAfterLeasing = state.calcNewBalances(Seq(lease), Map(), allowTemporaryNegative = false)
        state.applyChanges(balancesAfterLeasing)

        state.isValid(cancel, Int.MaxValue) shouldBe true

        // apply cancel lease tx
        val balancesAfterCancel = state.calcNewBalances(Seq(cancel), Map(), allowTemporaryNegative = false)
        state.applyChanges(balancesAfterCancel)

        state.isValid(cancel, Int.MaxValue) shouldBe false

        val (newSenderBalance, newSenderEffectiveBalance, newRecipientBalance, newRecipientEffectiveBalance) = senderAndRecipientStateBalances(lease)

        newSenderBalance shouldBe senderBalance - lease.fee - cancel.fee
        newSenderEffectiveBalance shouldBe senderEffectiveBalance - lease.fee - cancel.fee
        newRecipientBalance shouldBe recipientBalance
        newRecipientEffectiveBalance shouldBe recipientEffectiveBalance
      }
    }
  }

  property("Lease cancel transaction with other sender") {
    forAll(leaseAndCancelWithOtherSenderGenerator) { case (lease: LeaseTransaction, cancel: LeaseCancelTransaction) =>
      withRollbackTest {

        //set some balance
        val genes = GenesisTransaction.create(lease.sender, lease.amount + lease.fee + cancel.fee, lease.timestamp - 1).right.get
        state.applyChanges(state.calcNewBalances(Seq(genes), Map(), allowTemporaryNegative = false))

        // apply lease tx
        val balancesAfterLeasing = state.calcNewBalances(Seq(lease), Map(), allowTemporaryNegative = false)
        state.applyChanges(balancesAfterLeasing)

        state.isValid(cancel, Int.MaxValue) shouldBe false
      }
    }
  }

  property("Lease cancel transaction without lease") {
    forAll(leaseCancelGenerator) { cancel: LeaseCancelTransaction =>
      withRollbackTest {
        state.isValid(cancel, Int.MaxValue) shouldBe false
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

      state.validateAgainstState(issueTx2, Int.MaxValue) shouldBe an[Left[_, _]]
    }
  }

  property("Reissue asset") {
    forAll(issueReissueGenerator) { pair =>
      withRollbackTest {
        val issueTx: IssueTransaction = pair._1
        val reissueTx: ReissueTransaction = pair._3

        state.validateAgainstState(issueTx, Int.MaxValue) shouldBe an[Right[_, _]]

        state.applyChanges(state.calcNewBalances(Seq(issueTx), Map(), allowTemporaryNegative = true))

        state.validateAgainstState(issueTx, Int.MaxValue) shouldBe an[Left[_, _]]

        val state1 = state.validateAgainstState(reissueTx, Int.MaxValue)
        issueTx.reissuable match {
          case true => state1 shouldBe an[Right[_, _]]
          case false => state1 shouldBe an[Left[_, _]]
        }
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
        state invokePrivate applyChanges(Map(testAssetAcc -> (AccState(balance, balance), Seq(FeesStateChange(balance), tx, tx))),
          NTP.correctedTime())
        state.balance(testAcc) shouldBe balance
        state.assetBalance(testAssetAcc) shouldBe balance
        state.included(tx.id).value shouldBe state.stateHeight
        state invokePrivate applyChanges(Map(testAssetAcc -> (AccState(0L, 0L), Seq(tx))), NTP.correctedTime())
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
        state invokePrivate applyChanges(Map(assetAccount -> (AccState(balance, balance), Seq(FeesStateChange(balance)))),
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
        state invokePrivate applyChanges(Map(assetAccount -> (AccState(balance, balance), Seq(FeesStateChange(balance)))),
          NTP.correctedTime())
        state.assetBalance(assetAccount) shouldBe balance
        state.isValid(tx, System.currentTimeMillis) should be(false)
      }
    }
  }

  property("Order matching") {
    forAll { x: (ExchangeTransaction, PrivateKeyAccount) =>
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
        val buyer = om.buyOrder.senderPublicKey
        val seller = om.sellOrder.senderPublicKey

        val buyerAcc1 = AssetAcc(buyer, pair.first)
        val buyerAcc2 = AssetAcc(buyer, pair.second)
        val sellerAcc1 = AssetAcc(seller, pair.first)
        val sellerAcc2 = AssetAcc(seller, pair.second)
        val buyerFeeAcc = AssetAcc(buyer, None)
        val sellerFeeAcc = AssetAcc(seller, None)
        val matcherFeeAcc = AssetAcc(om.buyOrder.matcherPublicKey, None)

        val Seq(buyerBal1, buyerBal2, sellerBal1, sellerBal2, buyerFeeBal, sellerFeeBal, matcherFeeBal) =
          getBalances(buyerAcc1, buyerAcc2, sellerAcc1, sellerAcc2, buyerFeeAcc, sellerFeeAcc, matcherFeeAcc)

        state.applyChanges(state.calcNewBalances(Seq(om), Map(), allowTemporaryNegative = true))

        val Seq(newBuyerBal1, newBuyerBal2, newSellerBal1, newSellerBal2, newBuyerFeeBal, newSellerFeeBal, newMatcherFeeBal) =
          getBalances(buyerAcc1, buyerAcc2, sellerAcc1, sellerAcc2, buyerFeeAcc, sellerFeeAcc, matcherFeeAcc)

        newBuyerBal1 should be(buyerBal1 - (BigInt(om.amount) * om.price / Order.PriceConstant).toLong - feeInAsset(om.buyMatcherFee, buyerAcc1.assetId))
        newBuyerBal2 should be(buyerBal2 + om.amount -
          feeInAsset(om.buyMatcherFee, buyerAcc2.assetId))
        newSellerBal1 should be(sellerBal1 + (BigInt(om.amount) * om.price / Order.PriceConstant).toLong - feeInAsset(om.sellMatcherFee, sellerAcc1.assetId))
        newSellerBal2 should be(sellerBal2 - om.amount -
          feeInAsset(om.sellMatcherFee, sellerAcc2.assetId))
        newBuyerFeeBal should be(buyerFeeBal - om.buyMatcherFee + amountInWaves((BigInt(om.amount) * om.price / Order.PriceConstant).toLong, om.buyOrder))
        newSellerFeeBal should be(sellerFeeBal - om.sellMatcherFee + amountInWaves((BigInt(om.amount) * om.price / Order.PriceConstant).toLong, om.sellOrder))
        newMatcherFeeBal should be(matcherFeeBal + om.buyMatcherFee + om.sellMatcherFee - om.fee)
      }
    }
  }

  property("Reopen state") {
    val balance = 1234L
    state invokePrivate applyChanges(Map(testAssetAcc -> (AccState(balance, balance), Seq(FeesStateChange(balance)))),
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

  def profile[R](block: => R): (Long, R) = {
    val start = System.currentTimeMillis()
    val result = block // call-by-name
    (System.currentTimeMillis() - start, result)
  }

}
