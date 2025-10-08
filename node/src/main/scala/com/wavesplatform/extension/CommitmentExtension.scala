package com.wavesplatform.extension

import com.google.common.primitives.Ints
import com.wavesplatform.crypto
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Proofs, TxValidationError}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class CommitmentExtension(context: Context) extends Extension with ScorexLogging {
  private implicit val scheduler: Scheduler = Scheduler.singleThread("commitment-extension")

  override def start(): Unit = {
    log.info("Starting CommitmentExtension")
    if (context.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality)) {
      val settings               = context.settings
      val generationPeriodLength = settings.blockchainSettings.functionalitySettings.generationPeriodLength
      val wallet                 = context.wallet

      def createTask(generationPeriodStart: Height): Task[Unit] = Task {
        wallet.privateKeyAccounts.foreach { account =>
          val fee          = 10000000L
          val wavesBalance = context.blockchain.balance(account.toAddress)

          if (wavesBalance < fee) {
            log.error(s"Insufficient balance for fee for account ${account.toAddress}. Required: $fee, available: $wavesBalance")
          } else {
            if (wavesBalance < 100 * 100000000L) {
              log.warn(s"Balance is low for account ${account.toAddress}: $wavesBalance. It's less than 100 WAVES.")
            }

            val timestamp = context.time.getTimestamp()

            val blsKP      = BlsKeyPair(account.privateKey)
            val blsMessage = blsKP.publicKey.arr ++ Ints.toByteArray(generationPeriodStart)
            val blsSig     = blsKP.sign(blsMessage)

            val commitToGenTxE = CommitToGenerationTransaction
              .create(
                sender = account.publicKey,
                endorserPublicKey = blsKP.publicKey,
                generationPeriodStart = generationPeriodStart,
                timestamp = timestamp,
                feeInWaves = fee,
                commitmentSignature = blsSig,
                proofs = Proofs.empty,
                chainId = settings.blockchainSettings.addressSchemeCharacter.toByte
              )
              .map(tx => tx.copy(proofs = Proofs(crypto.sign(account.privateKey, tx.bodyBytes()))))

            commitToGenTxE.fold(
              {
                case TxValidationError.InsufficientFee =>
                  log.error(
                    s"Failed to create transaction for account ${account.toAddress} due to insufficient fee: ${TxValidationError.InsufficientFee}"
                  )
                case e => log.error(s"Failed to create CommitToGenerationTransaction for account ${account.toAddress}: $e")
              },
              tx => {
                log.info(s"Created CommitToGenerationTransaction with id: ${tx.id()} for account ${account.toAddress}")
                context.broadcastTransaction(tx).onComplete {
                  case Success(true) =>
                    log.info(s"Successfully broadcasted commitment for ${account.toAddress}")
                  case Success(false) =>
                    log.debug(s"Failed to broadcast commitment for ${account.toAddress}, maybe it already exists.")
                  case Failure(exception) =>
                    log.warn(s"Failed to broadcast commitment for ${account.toAddress}", exception)
                }
              }
            )
          }
        }
      }

      def waitForNewHeight(currentHeight: Int): Task[Int] = {
        def check(): Task[Int] = {
          val newHeight = context.blockchain.height
          if (newHeight > currentHeight) {
            Task.now(newHeight)
          } else {
            Task.sleep(7.seconds) >> check()
          }
        }
        check()
      }

      def heightLoop(h: Int): Task[Unit] = {
        waitForNewHeight(h).flatMap { newHeight =>
          val n = Math.floor((newHeight - 1).toDouble / generationPeriodLength).toLong
          val periodToCommit = (n + 1) * generationPeriodLength

          log.debug(s"Checking for commitments at height $newHeight for period $periodToCommit.")

          createTask(Height(periodToCommit.toInt)) >> heightLoop(newHeight)
        }
      }

      heightLoop(context.blockchain.height - 1).runAsyncLogErr
    } else {
      log.warn("DeterministicFinality feature is not activated. CommitmentExtension will not start.")
    }
  }

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down CommitmentExtension")
    Future.successful(())
  }
}