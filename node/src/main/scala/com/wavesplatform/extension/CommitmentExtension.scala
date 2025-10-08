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

class CommitmentExtension(context: Context) extends Extension with ScorexLogging {
  private implicit val scheduler: Scheduler = Scheduler.singleThread("commitment-extension")

  override def start(): Unit = {
    log.info("Starting CommitmentExtension")
    if (context.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality)) {
      val settings               = context.settings
      val generationPeriodLength = settings.blockchainSettings.functionalitySettings.generationPeriodLength
      val wallet                 = context.wallet

      def waitForHeight(height: Int): Task[Unit] = {
        def check(): Task[Unit] = {
          if (context.blockchain.height >= height) {
            Task.unit
          } else {
            Task.sleep(1.second) >> check()
          }
        }
        check()
      }

      def loop(n: Long): Task[Unit] = {
        val txSendHeight   = n * generationPeriodLength + 1
        val periodToCommit = (n + 1) * generationPeriodLength

        log.info(s"Waiting for height $txSendHeight to create commitment for period starting at $periodToCommit")

        for {
          _ <- waitForHeight(txSendHeight.toInt)
          _ <- createTask(Height(periodToCommit.toInt))
          _ <- loop(n + 1)
        } yield ()
      }

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
                log.info(s"BLS Public Key: ${blsKP.publicKey.base58}")
                log.info(s"Commitment Signature: ${blsSig.base58}")
                context.broadcastTransaction(tx)
              }
            )
          }
        }
      }

      val initialN = Math.floor((context.blockchain.height - 1).toDouble / generationPeriodLength).toLong
      loop(initialN).runAsyncLogErr
    } else {
      log.warn("DeterministicFinality feature is not activated. CommitmentExtension will not start.")
    }
  }

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down CommitmentExtension")
    Future.successful(())
  }
}
