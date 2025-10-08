package com.wavesplatform.extension

import com.google.common.primitives.Ints
import com.wavesplatform.account.Address
import com.wavesplatform.crypto
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Proofs, TxValidationError}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class CommitmentExtension(context: Context) extends Extension with ScorexLogging {
  private implicit val scheduler: Scheduler = Scheduler.singleThread("commitment-extension")

  // NOTE: This in-memory cache does not survive a restart.
  // On restart, commitments might be sent again for the current period.
  private val committedInPeriod = mutable.Map[Int, Set[Address]]()

  override def start(): Unit = {
    log.info("Starting CommitmentExtension")
    if (context.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality)) {
      val settings               = context.settings
      val generationPeriodLength = settings.blockchainSettings.functionalitySettings.generationPeriodLength
      val wallet                 = context.wallet

      def createTask(generationPeriodStart: Height, committedAccounts: Set[Address]): Task[Set[Address]] = Task {
        var newlyCommitted = Set.empty[Address]
        wallet.privateKeyAccounts.foreach { account =>
          if (!committedAccounts.contains(account.toAddress)) {
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
                  newlyCommitted += account.toAddress
                }
              )
            }
          }
        }
        newlyCommitted
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

          committedInPeriod.keys.foreach { period =>
            if (period < periodToCommit) {
              committedInPeriod.remove(period)
            }
          }

          val committedAccounts = committedInPeriod.getOrElse(periodToCommit.toInt, Set.empty)
          log.debug(s"Checking for commitments at height $newHeight for period $periodToCommit. Known commitments: ${committedAccounts.size}")

          createTask(Height(periodToCommit.toInt), committedAccounts).flatMap { newlyCommitted =>
            if (newlyCommitted.nonEmpty) {
              log.info(s"Committed for ${newlyCommitted.size} new accounts for period $periodToCommit")
              committedInPeriod(periodToCommit.toInt) = committedAccounts ++ newlyCommitted
            }
            heightLoop(newHeight)
          }
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