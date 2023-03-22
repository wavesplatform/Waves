package com.wavesplatform.test

import com.wavesplatform.{Exporter, checkGenesis, crypto}
import com.wavesplatform.Exporter.IO
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.openDB
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.BlockchainGenerator.{GenBlock, GenTx}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.{
  BurnTransaction,
  IssueTransaction,
  ReissueTransaction,
  SetAssetScriptTransaction,
  SponsorFeeTransaction,
  UpdateAssetInfoTransaction
}
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, EthereumTransaction, PaymentTransaction, Transaction, TxHelpers}
import com.wavesplatform.utils.{Schedulers, ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observer
import monix.reactive.subjects.ConcurrentSubject
import org.apache.commons.io.FileUtils
import org.web3j.crypto.{ECKeyPair, RawTransaction}

import java.io.BufferedOutputStream
import java.nio.file.Files
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.reflectiveCalls
import scala.util.{Failure, Success}

// @formatter:off
/** Usage example: <pre>object Example extends App {
 *  val wavesSettings = Application.loadApplicationConfig(Some(new File("path-to-config-file")))
 *  val generator = new BlockchainGenerator(wavesSettings)
 *  val sender = KeyPair("123".getBytes)
 *  val recipient = Address.fromString("3FddHK1Y3vPdcVKZshWCWea4gS5th6G1UE6").getOrElse(sender.toAddress)
 *  val genBlocks = (1 to 10).map { idx =>
 *    GenBlock(
 *      (1 to 5).map(txIdx => GenTx(TxHelpers.transfer(sender, recipient, amount = (idx * 10 + txIdx) * 100000000L), Right(sender))),
 *      signer = sender
 *    )
 *  }
 *  generator.generateBinaryFile(genBlocks)
 *
 *  // only if you use Application.loadApplicationConfig method to create WavesSettings object
 *  Try(Await.result(Kamon.stopModules(), 10.seconds))
 *  Metrics.shutdown()
 *}
 * </pre>
 */
// @formatter:on
class BlockchainGenerator(wavesSettings: WavesSettings) extends ScorexLogging {

  private val settings: WavesSettings = wavesSettings.copy(minerSettings = wavesSettings.minerSettings.copy(quorum = 0))

  def generateDb(genBlocks: Seq[GenBlock], dbDirPath: String = settings.dbSettings.directory): Unit =
    generateBlockchain(genBlocks, dbDirPath)

  def generateBinaryFile(genBlocks: Seq[GenBlock]): Unit = {
    val targetHeight = genBlocks.size + 1
    log.info(s"Exporting to $targetHeight")
    val outputFilename = s"blockchain-$targetHeight"
    log.info(s"Output file: $outputFilename")

    Exporter.IO.createOutputStream(outputFilename) match {
      case Success(output) =>
        val bos       = new BufferedOutputStream(output, 10 * 1024 * 1024)
        val dbDirPath = Files.createTempDirectory("generator-temp-db")
        generateBlockchain(genBlocks, dbDirPath.toString, block => IO.exportBlockToBinary(bos, Some(block), legacy = true))
        log.info(s"Finished exporting $targetHeight blocks")
        bos.close()
        output.close()
        FileUtils.deleteDirectory(dbDirPath.toFile)
      case Failure(ex) => log.error(s"Failed to create file '$outputFilename': $ex")
    }
  }

  private def generateBlockchain(genBlocks: Seq[GenBlock], dbDirPath: String, exportToFile: Block => Unit = _ => ()): Unit = {
    val scheduler = Schedulers.singleThread("appender")
    val time = new Time {
      val startTime: Long = settings.blockchainSettings.genesisSettings.timestamp

      @volatile
      var time: Long = startTime

      override def correctedTime(): Long = time
      override def getTimestamp(): Long  = time
    }
    val db = openDB(dbDirPath)
    val (blockchain, _) =
      StorageFactory(settings, db, time, Observer.empty, BlockchainUpdateTriggers.noop)
    val utxPool     = new UtxPoolImpl(time, blockchain, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
    val pos         = PoSSelector(blockchain, settings.synchronizationSettings.maxBaseTarget)
    val extAppender = BlockAppender(blockchain, time, utxPool, pos, scheduler) _
    val utxEvents   = ConcurrentSubject.publish[UtxEvent]

    val miner = new MinerImpl(
      new DefaultChannelGroup("", null),
      blockchain,
      settings,
      time,
      utxPool,
      Wallet(settings.walletSettings),
      PoSSelector(blockchain, None),
      scheduler,
      scheduler,
      utxEvents.collect { case _: UtxEvent.TxAdded =>
        ()
      }
    )

    checkGenesis(settings, blockchain, Miner.Disabled)
    val result = genBlocks.foldLeft[Either[ValidationError, Unit]](Right(())) {
      case (res @ Left(_), _) => res
      case (_, genBlock) =>
        time.time = miner.nextBlockGenerationTime(blockchain, blockchain.height, blockchain.lastBlockHeader.get, genBlock.signer).explicitGet()
        val correctedTimeTxs = genBlock.txs.map(correctTxTimestamp(_, time))

        miner.forgeBlock(genBlock.signer) match {
          case Right((block, _)) =>
            for {
              blockWithTxs <- Block.buildAndSign(
                block.header.version,
                block.header.timestamp,
                block.header.reference,
                block.header.baseTarget,
                block.header.generationSignature,
                correctedTimeTxs,
                genBlock.signer,
                block.header.featureVotes,
                block.header.rewardVote
              )
              _ <- Await
                .result(extAppender(blockWithTxs).runAsyncLogErr, Duration.Inf)
            } yield exportToFile(blockWithTxs)

          case Left(err) => Left(GenericError(err))
        }
    }
    result match {
      case Right(_) =>
        if (blockchain.isFeatureActivated(BlockchainFeatures.NG) && blockchain.liquidBlockMeta.nonEmpty) {
          val lastHeader = blockchain.lastBlockHeader.get.header
          val pseudoBlock = Block(
            BlockHeader(
              blockchain.blockVersionAt(blockchain.height),
              time.getTimestamp() + settings.blockchainSettings.genesisSettings.averageBlockDelay.toMillis,
              blockchain.lastBlockId.get,
              lastHeader.baseTarget,
              lastHeader.generationSignature,
              lastHeader.generator,
              Nil,
              0,
              ByteStr.empty
            ),
            ByteStr.empty,
            Nil
          )
          blockchain.processBlock(pseudoBlock, ByteStr.empty, verify = false)
        }
      case Left(err) => log.error(s"Error appending block: $err")
    }

  }

  private def correctTxTimestamp(genTx: GenTx, time: Time): Transaction =
    genTx match {
      case GenTx(t: BurnTransaction, Right(signer))        => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: CreateAliasTransaction, Right(signer)) => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: DataTransaction, Right(signer))        => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: EthereumTransaction, Left(signer)) =>
        val correctedTimeRawTx = RawTransaction.createTransaction(
          BigInt(time.getTimestamp()).bigInteger,
          t.underlying.getGasPrice,
          t.underlying.getGasLimit,
          t.underlying.getTo,
          t.underlying.getValue,
          t.underlying.getData
        )
        EthTxGenerator.signRawTransaction(signer, t.chainId)(correctedTimeRawTx)
      case GenTx(t: ExchangeTransaction, Right(signer))     => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: InvokeScriptTransaction, Right(signer)) => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: IssueTransaction, Right(signer))        => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: LeaseCancelTransaction, Right(signer))  => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: LeaseTransaction, Right(signer))        => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: MassTransferTransaction, Right(signer)) => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: PaymentTransaction, Right(signer)) =>
        t.copy(timestamp = time.getTimestamp(), signature = crypto.sign(signer.privateKey, t.bodyBytes()))
      case GenTx(t: ReissueTransaction, Right(signer))         => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: SetAssetScriptTransaction, Right(signer))  => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: SetScriptTransaction, Right(signer))       => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: SponsorFeeTransaction, Right(signer))      => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: TransferTransaction, Right(signer))        => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t: UpdateAssetInfoTransaction, Right(signer)) => t.copy(timestamp = time.getTimestamp()).signWith(signer.privateKey)
      case GenTx(t, _)                                         => t
    }
}

object BlockchainGenerator {
  case class GenBlock(
      txs: Seq[GenTx],
      signer: KeyPair = TxHelpers.defaultSigner,
      version: Byte = Block.ProtoBlockVersion
  )
  case class GenTx(
      tx: Transaction,
      signer: Either[ECKeyPair, KeyPair]
  )
}
