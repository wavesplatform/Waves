package scorex.transaction

import akka.actor.ActorRef
import com.google.common.base.Charsets
import com.wavesplatform.history.BlockStorageImpl
import com.wavesplatform.settings.{GenesisSettings, WavesSettings}
import com.wavesplatform.state2.Validator
import scorex.account._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash.{DigestSize, hash}
import scorex.network.message.Message
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, _}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils._
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPaymentRequest

import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Left, Right}


class SimpleTransactionModule(val settings: WavesSettings, networkController: ActorRef)
  extends TransactionModule with TransactionOperations with ScorexLogging {

  import SimpleTransactionModule._

  private val feeCalculator = new FeeCalculator(settings.feesSettings)
  private val fs = settings.blockchainSettings.functionalitySettings

  val utxStorage: UnconfirmedTransactionsStorage = new UnconfirmedTransactionsDatabaseImpl(settings.utxSettings)

  override val blockStorage = new BlockStorageImpl(settings.blockchainSettings)

  override def unconfirmedTxs: Seq[Transaction] = utxStorage.all()

  override def putUnconfirmedIfNew[T <: Transaction](tx: T): Either[ValidationError, T] = synchronized {
    for {
      t1 <- feeCalculator.enoughFee(tx)
      t2 <- utxStorage.putIfNew(t1, (t: T) => validate(t))
    } yield t2
  }

  override def packUnconfirmed(heightOpt: Option[Int]): Seq[Transaction] = synchronized {
    clearIncorrectTransactions()

    val txs = utxStorage.all()
      .sorted(TransactionsOrdering.InUTXPool)
      .take(MaxTransactionsPerBlock)
      .sorted(TransactionsOrdering.InBlock)

    val valid = Validator.validate(fs, blockStorage.stateReader, txs, heightOpt, NTP.correctedTime())._2

    if (valid.size != txs.size) {
      log.debug(s"Txs for new block do not match: valid=${valid.size} vs all=${txs.size}")
    }

    valid
  }

  override def clearFromUnconfirmed(data: Seq[Transaction]): Unit = synchronized {
    data.foreach(tx => utxStorage.getBySignature(tx.id) match {
      case Some(unconfirmedTx) => utxStorage.remove(unconfirmedTx)
      case None =>
    })

    clearIncorrectTransactions() // todo makes sence to remove expired only at this point
  }

  def clearIncorrectTransactions(): Unit = {
    val currentTime = NTP.correctedTime()
    val txs = utxStorage.all()
    val notExpired = txs.filter { tx => (currentTime - tx.timestamp).millis <= MaxTimeUtxPast }
    val notFromFuture = notExpired.filter { tx => (tx.timestamp - currentTime).millis <= MaxTimeUtxFuture }
    val inOrder = notFromFuture.sorted(TransactionsOrdering.InUTXPool)
    val valid = Validator.validate(fs, blockStorage.stateReader, inOrder, None, currentTime)._2
    // remove non valid or expired from storage
    txs.diff(valid).foreach(utxStorage.remove)
  }

  val consensusGenesisData: NxtLikeConsensusBlockData = NxtLikeConsensusBlockData(settings.blockchainSettings.genesisSettings.initialBaseTarget, EmptySignature)

  private val MinBlocktimeLimit = normalize(53)
  private val MaxBlocktimeLimit = normalize(67)
  private val BaseTargetGamma = normalize(64)
  private val MaxBaseTarget = Long.MaxValue / avgDelayInSeconds

  private def avgDelayInSeconds: Long = settings.blockchainSettings.genesisSettings.averageBlockDelay.toSeconds

  private def normalize(value: Long): Double = value * avgDelayInSeconds / (60: Double)

  def blockOrdering: Ordering[(Block)] =
    Ordering.by {
      block =>
        val parent = blockStorage.history.blockById(block.reference).get
        val blockCreationTime = nextBlockGenerationTime(parent, block.signerData.generator)
          .getOrElse(block.timestamp)

        (block.blockScore, -blockCreationTime)
    }

  def isValid(block: Block): Boolean = try {
    val blockTime = block.timestampField.value

    require((blockTime - NTP.correctedTime()).millis < MaxTimeDrift, s"Block timestamp $blockTime is from future")

    val history = blockStorage.history

    if (block.timestampField.value > settings.blockchainSettings.functionalitySettings.requireSortedTransactionsAfter) {
      require(block.transactionDataField.asInstanceOf[TransactionsBlockField].value.sorted(TransactionsOrdering.InBlock) == block.transactionDataField.asInstanceOf[TransactionsBlockField].value, "Transactions must be sorted correctly")
    }

    val parentOpt = history.parent(block)
    require(parentOpt.isDefined || history.height() == 1, s"Can't find parent block with id '${
      Base58.encode(block.referenceField.value)
    }' of block " +
      s"'${
        Base58.encode(block.uniqueId)
      }'")

    val parent = parentOpt.get
    val parentHeightOpt = history.heightOf(parent.uniqueId)
    require(parentHeightOpt.isDefined, s"Can't get parent block with id '${
      Base58.encode(block.referenceField.value)
    }' height")
    val parentHeight = parentHeightOpt.get

    val prevBlockData = parent.consensusDataField.value
    val blockData = block.consensusDataField.value

    //check baseTarget
    val cbt = calcBaseTarget(parent, blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Block's basetarget is wrong, calculated: $cbt, block contains: $bbt")

    val generator = block.signerDataField.value.generator

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.sameElements(blockGs),
      s"Block's generation signature is wrong, calculated: ${
        calcGs.mkString
      }, block contains: ${
        blockGs.mkString
      }")

    val effectiveBalance = generatingBalance(generator, parentHeight)

    if (block.timestampField.value >= settings.blockchainSettings.functionalitySettings.minimalGeneratingBalanceAfterTimestamp) {
      require(effectiveBalance >= MinimalEffectiveBalanceForGenerator, s"Effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
    }

    //check hit < target
    calcHit(prevBlockData, generator) < calcTarget(parent, blockTime, effectiveBalance)
  } catch {
    case e: IllegalArgumentException =>
      log.error("Error while checking a block", e)
      false
    case NonFatal(t) =>
      log.error("Fatal error while checking a block", t)
      throw t
  }

  def generateNextBlock(account: PrivateKeyAccount): Option[Block] = try {

    val lastBlock = blockStorage.history.lastBlock
    val height = blockStorage.history.heightOf(lastBlock).get
    val balance = generatingBalance(account, height)

    if (balance < MinimalEffectiveBalanceForGenerator) {
      throw new IllegalStateException(s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
    }

    val lastBlockKernelData = lastBlock.consensusDataField.value

    val lastBlockTime = lastBlock.timestampField.value

    val currentTime = NTP.correctedTime()

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(lastBlock, currentTime, balance)

    val eta = (currentTime - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${
      h < t
    }, eta $eta, " +
      s"account:  $account " +
      s"account balance: $balance " +
      s"last block id: ${
        lastBlock.encodedId
      }, " +
      s"height: $height, " +
      s"last block target: ${
        lastBlockKernelData.baseTarget
      }"
    )

    if (h < t) {

      val btg = calcBaseTarget(lastBlock, currentTime)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      val consensusData = NxtLikeConsensusBlockData(btg, gs)

      val unconfirmed = packUnconfirmed(Some(height))
      log.debug(s"Build block with ${
        unconfirmed.size
      } transactions")
      log.debug(s"Block time interval is $eta seconds ")

      Some(Block.buildAndSign(Version,
        currentTime,
        lastBlock.uniqueId,
        consensusData,
        unconfirmed,
        account))
    } else None
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      None
    case e: IllegalStateException =>
      log.debug(s"Failed to generate new block: ${
        e.getMessage
      }")
      None
  }

  def nextBlockGenerationTime(block: Block, account: PublicKeyAccount): Option[Long] = {
    blockStorage.history.heightOf(block.uniqueId)
      .map(height => (height, generatingBalance(account, height))).filter(_._2 > 0)
      .flatMap {
        case (height, balance) =>
          val cData = block.consensusData
          val hit = calcHit(cData, account)
          val t = cData.baseTarget

          val result =
            Some((hit * 1000) / (BigInt(t) * balance) + block.timestamp)
              .filter(_ > 0).filter(_ < Long.MaxValue)
              .map(_.toLong)

          log.debug({
            val currentTime = NTP.correctedTime()
            s"Next block gen time: $result " +
              s"in ${
                result.map(t => (t - currentTime) / 1000)
              } seconds, " +
              s"hit: $hit, target: $t, " +
              s"account:  $account, account balance: $balance " +
              s"last block id: ${
                block.encodedId
              }, " +
              s"height: $height"
          })

          result
      }
  }

  private def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount) =
    hash(lastBlockData.generationSignature ++ generator.publicKey)

  private def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8).reverse)

  /**
    * BaseTarget calculation algorithm fixing the blocktimes.
    */
  private def calcBaseTarget[TT](prevBlock: Block, timestamp: Long): Long = {
    val history = blockStorage.history
    val height = history.heightOf(prevBlock).get
    val prevBaseTarget = prevBlock.consensusDataField.value.baseTarget
    if (height % 2 == 0) {
      val blocktimeAverage = history.parent(prevBlock, AvgBlockTimeDepth - 1)
        .map(b => (timestamp - b.timestampField.value) / AvgBlockTimeDepth)
        .getOrElse(timestamp - prevBlock.timestampField.value) / 1000

      val baseTarget = (if (blocktimeAverage > avgDelayInSeconds) {
        prevBaseTarget * Math.min(blocktimeAverage, MaxBlocktimeLimit) / avgDelayInSeconds
      } else {
        prevBaseTarget - prevBaseTarget * BaseTargetGamma *
          (avgDelayInSeconds - Math.max(blocktimeAverage, MinBlocktimeLimit)) / (avgDelayInSeconds * 100)
      }).toLong

      // TODO: think about MinBaseTarget like in Nxt
      scala.math.min(baseTarget, MaxBaseTarget)
    } else {
      prevBaseTarget
    }
  }

  protected def calcTarget(prevBlock: Block,
                           timestamp: Long,
                           balance: Long): BigInt = {

    require(balance >= 0, s"Balance cannot be negative")

    val prevBlockData = prevBlock.consensusDataField.value
    val prevBlockTimestamp = prevBlock.timestampField.value

    val eta = (timestamp - prevBlockTimestamp) / 1000 //in seconds

    BigInt(prevBlockData.baseTarget) * eta * balance
  }

  def generatingBalance(account: Account, atHeight: Int): Long = {
    val generatingBalanceDepth =
      if (atHeight >= settings.blockchainSettings.functionalitySettings.generatingBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    blockStorage.stateReader.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

  override def onNewOffchainTransaction[T <: Transaction](transaction: T): Either[ValidationError, T] =
    for {
      tx <- putUnconfirmedIfNew(transaction)
    } yield {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
      tx
    }

  override def createPayment(request: PaymentRequest, wallet: Wallet): Either[ValidationError, PaymentTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    rec <- Account.fromString(request.recipient)
    pmt <- createPayment(pk, rec, request.amount, request.fee)
  } yield pmt


  override def transferAsset(request: TransferRequest, wallet: Wallet): Either[ValidationError, TransferTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      recipientAcc <- AccountOrAlias.fromString(request.recipient)
      tx <- TransferTransaction
        .create(request.assetId.map(s => Base58.decode(s).get),
          senderPrivateKey,
          recipientAcc,
          request.amount,
          getTimestamp,
          request.feeAssetId.map(s => Base58.decode(s).get),
          request.fee,
          request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))
      r <- onNewOffchainTransaction(tx)
    } yield r

  override def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction] =
    for {
      senderPrivateKey <- wallet.findWallet(request.sender)
      tx <- IssueTransaction.create(senderPrivateKey,
        request.name.getBytes(Charsets.UTF_8),
        request.description.getBytes(Charsets.UTF_8),
        request.quantity, request.decimals, request.reissuable, request.fee, getTimestamp)
      r <- onNewOffchainTransaction(tx)
    } yield r

  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    recipientAcc <- AccountOrAlias.fromString(request.recipient)
    tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, getTimestamp, recipientAcc)
    r <- onNewOffchainTransaction(tx)
  } yield r

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findWallet(request.sender)
      lease <- LeaseCancelTransaction.create(pk, Base58.decode(request.txId).get, request.fee, getTimestamp)
      t <- onNewOffchainTransaction(lease)
    } yield t


  override def alias(request: CreateAliasRequest, wallet: Wallet): Either[ValidationError, CreateAliasTransaction] = for {
    senderPrivateKey <- wallet.findWallet(request.sender)
    alias <- Alias.buildWithCurrentNetworkByte(request.alias)
    tx <- CreateAliasTransaction.create(senderPrivateKey, alias, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r

  override def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- ReissueTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.reissuable, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r


  override def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction] = for {
    pk <- wallet.findWallet(request.sender)
    tx <- BurnTransaction.create(pk, Base58.decode(request.assetId).get, request.quantity, request.fee, getTimestamp)
    r <- onNewOffchainTransaction(tx)
  } yield r


  private var txTime: Long = 0

  private def getTimestamp: Long = synchronized {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  override def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction] =
    PaymentTransaction.create(sender, recipient, amount, fee, getTimestamp)
      .flatMap(onNewOffchainTransaction)


  /** Check whether tx is valid on current state and not expired yet
    */
  override def validate[T <: Transaction](tx: T): Either[ValidationError, T] = try {
    val lastBlockTimestamp = blockStorage.history.lastBlock.timestamp
    val notExpired = (lastBlockTimestamp - tx.timestamp).millis <= MaxTimePreviousBlockOverTransactionDiff
    if (notExpired) {
      Validator.validate(fs, blockStorage.stateReader, tx)
    } else {
      Left(TransactionValidationError(tx, s"Transaction is too old: Last block timestamp is $lastBlockTimestamp"))
    }
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      Left(TransactionValidationError(tx, "DB can't find last block because of unexpected modification"))
    case NonFatal(t) =>
      log.error(s"Unexpected error during validation", t)
      throw t
  }

  override def createPayment(sender: PrivateKeyAccount, recipient: Account,
                             amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] =
    PaymentTransaction.create(sender, recipient, amount, fee, timestamp)

  override def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction] =
    for {
      _signature <- Base58.decode(payment.signature).toOption.toRight(ValidationError.InvalidSignature)
      _sender <- PublicKeyAccount.fromBase58String(payment.senderPublicKey)
      _recipient <- Account.fromString(payment.recipient)
      _t <- PaymentTransaction.create(_sender, _recipient, payment.amount, payment.fee, payment.timestamp, _signature)
      t <- onNewOffchainTransaction(_t)
    } yield t
}

object SimpleTransactionModule {

  val MaxTimeUtxFuture: FiniteDuration = 15.seconds
  val MaxTimeUtxPast: FiniteDuration = 90.minutes
  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePreviousBlockOverTransactionDiff: FiniteDuration = 90.minutes
  val MaxTimeCurrentBlockOverTransactionDiff: FiniteDuration = 2.hour
  val MaxTransactionsPerBlock: Int = 100
  val BaseTargetLength: Int = 8
  val GeneratorSignatureLength: Int = 32
  val MinimalEffectiveBalanceForGenerator: Long = 1000000000000L
  val AvgBlockTimeDepth: Int = 3
  val MaxTimeDrift: FiniteDuration = 15.seconds
  val EmptySignature: Array[Byte] = Array.fill(DigestSize)(0: Byte)
  val Version: Byte = 2


  def buildTransactions(genesisSettings: GenesisSettings): Seq[GenesisTransaction] = {
    genesisSettings.transactions.map { ts =>
      val acc = Account.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, genesisSettings.transactionsTimestamp).right.get
    }
  }

}
