package scorex.lagonaki.unit

import java.nio.file.{Files, Paths}

import org.h2.mvstore.MVStore
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{Account, PrivateKeyAccount}
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.ChainParameters
import scorex.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.extension.OrderMatchStoredState
import scorex.transaction.state.wallet.{IssueRequest, TransferRequest}
import scorex.transaction.{AssetAcc, AssetId, GenesisTransaction, TransactionGen}
import scorex.utils.{ByteArrayExtension, NTP}
import scorex.wallet.Wallet

class OrderMatchStoredStateSpecification extends FunSuite with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  lazy val stateFile = Files.createTempFile(Paths.get("/tmp/"), "state", "dat")
  val wallet = new Wallet(None, "123", Some(Array(0.toByte, 1.toByte)))
  val accounts = wallet.generateNewAccounts(3)
  val acc1 = accounts.head
  val acc2 = accounts(1)
  val matcher = accounts(2)
  val WAVES_UNITS = Order.PriceConstant

  val db = new MVStore.Builder().fileName(stateFile.toString).compress().open()
  val state = StoredState.fromDB(db, ChainParameters.Enabled)
  state.applyBlock(TestBlock(Seq(GenesisTransaction.create(acc1, 1000 * WAVES_UNITS, 0).right.get,
    GenesisTransaction.create(acc2, 100 * WAVES_UNITS, 0).right.get)))

  override protected def afterAll(): Unit = {
    db.close()
    Files.deleteIfExists(stateFile)
  }

  var beforeStateHeigh = 0

  override protected def beforeEach(): Unit = {
    beforeStateHeigh = state.stateHeight
  }

  override protected def afterEach(): Unit = {
    state.rollbackTo(beforeStateHeigh)
  }

  def getTimestamp: Long = {
    System.currentTimeMillis
  }

  private def issueAsset(request: IssueRequest, wallet: Wallet): IssueTransaction = {
    val sender = wallet.privateKeyAccount(request.sender).get
    IssueTransaction.create(sender,
      request.name.getBytes,
      request.description.getBytes,
      request.quantity,
      request.decimals,
      request.reissuable,
      request.fee,
      getTimestamp).right.get
  }

  def transferAsset(request: TransferRequest, wallet: Wallet): TransferTransaction = {
    val sender = wallet.privateKeyAccount(request.sender).get
    TransferTransaction.create(request.assetId.map(s => Base58.decode(s).get),
      sender: PrivateKeyAccount,
      new Account(request.recipient),
      request.amount,
      getTimestamp,
      request.feeAssetId.map(s => Base58.decode(s).get),
      request.fee,
      Base58.decode(request.attachment).get).right.get
  }

  def createExchangeTransaction(buyOrder: Order, sellOrder: Order, price: Long, amount: Long,
                                buyMatcherFee: Long, sellMatcherFee: Long, fee: Long) =
    ExchangeTransaction.create(matcher, buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, getTimestamp) match {
      case Right(o) => o
      case Left(error) => fail(s"Error creating order: $error")
    }

  def createExchangeTransaction(buyOrder: Order, sellOrder: Order, price: Long, amount: Long, fee: Long) = {
    val buyMatcherFee: Long = buyOrder.matcherFee * amount / buyOrder.amount
    val sellMatcherFee: Long = sellOrder.matcherFee * amount / sellOrder.amount
    ExchangeTransaction.create(matcher, buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, getTimestamp) match {
      case Right(o) => o
      case Left(error) => fail(s"Error creating order: $error")
    }
  }

  def addInitialAssets(acc: PrivateKeyAccount, assetName: String, amount: Long): Option[AssetId] = {
    val issueAssetTx = issueAsset(IssueRequest(acc.address, Base58.encode(assetName.getBytes),
      Base58.encode(assetName.getBytes), amount * WAVES_UNITS, 2, reissuable = false, 1 * WAVES_UNITS), wallet)
    state.applyBlock(TestBlock(Seq(issueAssetTx))) should be('success)
    Some(issueAssetTx.assetId)
  }

  private def getBalances(acc: Account, pair: AssetPair) = {
    (state.assetBalance(AssetAcc(acc, None)), state.assetBalance(AssetAcc(acc, pair.first)),
      state.assetBalance(AssetAcc(acc, pair.second)))
  }

  private def initPairWithBalances(): (AssetPair, PrivateKeyAccount, PrivateKeyAccount) = {
    val usd = addInitialAssets(acc1, "usd", 1000L)
    val eur = addInitialAssets(acc2, "eur", 100L)

    val pair = AssetPair(usd, eur)
    val buyAcc = if (ByteArrayExtension.sameOption(usd, pair.first)) acc1 else acc2
    val sellAcc = if (ByteArrayExtension.sameOption(usd, pair.first)) acc2 else acc1
    (pair, buyAcc, sellAcc)
  }

  private def withCheckBalances(pair: AssetPair,
                                buyAcc: PrivateKeyAccount,
                                sellAcc: PrivateKeyAccount,
                                om: ExchangeTransaction)(f: => Unit): Unit = {
    val (prevBuyW, prevBuy1, prevBuy2) = getBalances(buyAcc, pair)
    val (prevSellW, prevSell1, prevSell2) = getBalances(sellAcc, pair)

    f

    //buyAcc buy1
    val (buyW, buy1, buy2) = getBalances(buyAcc, pair)
    buyW should be(prevBuyW - om.buyMatcherFee)
    buy1 should be(prevBuy1 - om.amount)
    buy2 should be(prevBuy2 + BigInt(om.amount) * Order.PriceConstant / om.price)

    //sellAcc sell1
    val (sellW, sell1, sell2) = getBalances(sellAcc, pair)
    sellW should be(prevSellW - om.sellMatcherFee)
    sell1 should be(prevSell1 + om.amount)
    sell2 should be(prevSell2 - BigInt(om.amount) * Order.PriceConstant / om.price)
  }

  test("Validate enough order balances") {
    val (pair, buyAcc, sellAcc) = initPairWithBalances()
    val (initBuyW, initBuy1, initBuy2) = getBalances(buyAcc, pair)
    val (initSellW, initSell1, initSell2) = getBalances(sellAcc, pair)
    val matcherTxFee = 1000L
    val price = 20 * Order.PriceConstant

    val buy1 = Order
      .buy(buyAcc, matcher, pair, price, 10 * WAVES_UNITS, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val sell1 = Order
      .sell(sellAcc, matcher, pair, price, 5 * WAVES_UNITS, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val buy1Fee = (0.5 * WAVES_UNITS).toLong
    val om1 = createExchangeTransaction(buy1, sell1, price, 5 * WAVES_UNITS, buy1Fee, sell1.matcherFee, matcherTxFee)

    state.isValid(om1, om1.timestamp) should be(true)
    state.applyBlock(TestBlock(Seq(om1))) should be('success)

    //buyAcc buy1
    val (om1buyW, om1buy1, om1buy2) = getBalances(buyAcc, pair)
    om1buyW should be(initBuyW - buy1Fee)
    om1buy1 should be(initBuy1 - 5 * Order.PriceConstant)
    om1buy2 should be(initBuy2 + BigInt(om1.amount) * Order.PriceConstant / price)

    //sellAcc sell1
    val (om1sellW, om1sell1, om1sell2) = getBalances(sellAcc, pair)
    om1sellW should be(initSellW - sell1.matcherFee)
    om1sell1 should be(initSell1 + 5 * Order.PriceConstant)
    om1sell2 should be(initSell2 - BigInt(om1.amount) * Order.PriceConstant / price)

    val sell2 = Order
      .sell(sellAcc, matcher, pair, price, 6 * WAVES_UNITS, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val notEnoughRemainingFromPrevOm = createExchangeTransaction(buy1, sell2, price, 6 * WAVES_UNITS, buy1Fee, sell1.matcherFee,
      matcherTxFee)

    state.isValid(notEnoughRemainingFromPrevOm, notEnoughRemainingFromPrevOm.timestamp) should be(false)

    val buy2 = Order.buy(buyAcc, matcher, pair, price, om1buy1 + 1, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val sell3 = Order
      .sell(sellAcc, matcher, pair, price, om1buy1 + 1, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val notEnoughBalOm = createExchangeTransaction(buy2, sell3, price, om1buy1 + 1, matcherTxFee)

    state.isValid(notEnoughBalOm, notEnoughBalOm.timestamp) should be(false)
    state.applyBlock(TestBlock(Seq(notEnoughBalOm))) should be('failure)

    val sell4 = Order
      .sell(sellAcc, matcher, pair, price, 5 * Order.PriceConstant, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val om2 = createExchangeTransaction(buy1, sell4, price, 5 * Order.PriceConstant, buy1.matcherFee - buy1Fee,
      sell4.matcherFee, matcherTxFee)

    state.isValid(om2, om2.timestamp) should be(true)
    state.applyBlock(TestBlock(Seq(om2))) should be('success)

    //buyAcc buy1 - executed om2
    val (om2buyW, om2buy1, om2buy2) = getBalances(buyAcc, pair)
    om2buyW should be(initBuyW - buy1.matcherFee)
    om2buy1 should be(om1buy1 - om2.amount)
    om2buy2 should be(om1buy2 + BigInt(om2.amount) * Order.PriceConstant / price)

    //sellAcc buy1 - executed om2
    val (om2sellW, om2sell1, om2sell2) = getBalances(sellAcc, pair)
    om2sellW should be(om1sellW - sell4.matcherFee)
    om2sell1 should be(om1sell1 + om2.amount)
    om2sell2 should be(om1sell2 - BigInt(om2.amount) * Order.PriceConstant / price)
  }

  test("Validate not enough balances for fee") {
    val (pair, buyAcc, sellAcc) = initPairWithBalances()
    val (prevBuyBalW, prevBuyBal1, prevBuyBal2) = getBalances(buyAcc, pair)
    val (prevSellBalW, prevSellBal1, prevSellBal2) = getBalances(sellAcc, pair)

    val matcherTxFee = 1000L
    val price = 20 * Order.PriceConstant

    val buy = Order
      .buy(buyAcc, matcher, pair, price, 5 * Order.PriceConstant, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val sell = Order
      .sell(sellAcc, matcher, pair, price, 10 * Order.PriceConstant, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)
    val sellFee = (0.5 * WAVES_UNITS).toLong
    val buyFee = 1 * WAVES_UNITS

    // Spend waves on buy acc
    val spendTx = transferAsset(TransferRequest(None, None, prevBuyBalW - buyFee, 1,
      buyAcc.address, "spend", sellAcc.address), wallet)
    //state.processBlock(new BlockMock(Seq(spendTx))) should be('success)

    val validOm = createExchangeTransaction(buy, sell, price, 5 * Order.PriceConstant, buyFee, sellFee, matcherTxFee)

    state.isValid(spendTx, spendTx.timestamp) should be(true)
    state.isValid(validOm, validOm.timestamp) should be(true)
    state.applyBlock(TestBlock(Seq(spendTx, validOm))) should be('failure)

    state.applyBlock(TestBlock(Seq(spendTx))) should be('success)
    state.isValid(validOm, validOm.timestamp) should be(false)

  }

  test("Partially execute sell order") {
    val (pair, buyAcc, sellAcc) = initPairWithBalances()
    val (initBuyW, initBuy1, initBuy2) = getBalances(buyAcc, pair)
    val (initSellW, initSell1, initSell2) = getBalances(sellAcc, pair)

    val matcherTxFee = 1000L
    val price = 20 * Order.PriceConstant

    val buy = Order
      .buy(buyAcc, matcher, pair, price, 10 * Order.PriceConstant, getTimestamp, getTimestamp + Order.MaxLiveTime - 1000,
        1 * WAVES_UNITS)
    val sell = Order
      .sell(sellAcc, matcher, pair, price, 100 * Order.PriceConstant, getTimestamp, getTimestamp + Order.MaxLiveTime, 1 * WAVES_UNITS)

    (1 to 11).foreach { i =>
      val aBuy = Order.sign(buy.copy(expiration = buy.expiration + i), buyAcc)
      val om = createExchangeTransaction(aBuy, sell, price, aBuy.amount, matcherTxFee)

      if (i < 11) {
        withCheckBalances(pair, buyAcc, sellAcc, om) {
          state.isValid(om, om.timestamp) should be(true)
          state.applyBlock(TestBlock(Seq(om))) should be('success)
        }
      } else state.isValid(om, om.timestamp) should be(false)
    }


  }
}

class OrderMatchTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("validates ExchangeTransaction against previous partial matches") {
    forAll(accountGen, maxWavesAnountGen, maxWavesAnountGen, maxWavesAnountGen, maxWavesAnountGen, maxWavesAnountGen) {
      (acc: PrivateKeyAccount, buyAmount: Long, sellAmount: Long, mf1: Long, mf2: Long, mf3: Long) =>
        whenever(buyAmount < sellAmount && BigInt(mf2) * buyAmount / sellAmount > 0) {
          var pairOption = Option.empty[AssetPair]
          while (pairOption.isEmpty) {
            pairOption = assetPairGen.sample
          }
          val pair = pairOption.get
          val sender1 = accountGen.sample.get
          val sender2 = accountGen.sample.get
          val matcher = accountGen.sample.get
          val curTime = NTP.correctedTime()
          val expired = curTime + 100 * 1000

          val buyPrice = sellAmount
          val sellPrice = buyAmount

          val buy1 = Order.buy(sender1, matcher, pair, buyPrice, buyAmount, curTime, expired, mf1)
          val sell = Order.sell(sender2, matcher, pair, sellPrice, sellAmount, curTime, expired, mf2)
          val buy2 = Order.buy(sender1, matcher, pair, buyPrice, sellAmount - buyAmount, curTime, expired, mf3)

          val om1 = ExchangeTransaction.create(acc, buy1, sell, buyPrice, buyAmount, mf1,
            (BigInt(mf2) * buyAmount / BigInt(sellAmount)).toLong, 1, curTime).right.get


          val om2 = ExchangeTransaction.create(acc, buy2, sell, buyPrice, sellAmount - om1.amount,
            mf3, mf2 - (BigInt(mf2) * buyAmount / sellAmount).toLong, 1, curTime).right.get

          // we should spent all fee
          val om2Invalid = ExchangeTransaction.create(acc, buy2, sell, buyPrice, sellAmount - om1.amount,
            mf3, (mf2 - (BigInt(mf2) * buyAmount / sellAmount).toLong) + 1, 1, curTime).right.get

          OrderMatchStoredState.isOrderMatchValid(om2Invalid, Set(om1)) shouldBe false
        }
    }

  }
}
