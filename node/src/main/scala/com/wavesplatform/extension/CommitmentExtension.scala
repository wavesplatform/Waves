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
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class CommitmentExtension(context: Context) extends Extension with ScorexLogging {
  private implicit val scheduler: Scheduler = Scheduler.singleThread("commitment-extension")

  private val committedAccounts = mutable.Set[String]()
  private var currentPeriod = -1

  override def start(): Unit = {
    log.info("Starting CommitmentExtension")
    if (context.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality)) {
      val settings               = context.settings
      val generationPeriodLength = settings.blockchainSettings.functionalitySettings.generationPeriodLength
      val wallet                 = context.wallet

      def createTask(generationPeriodStart: Height): Task[Unit] = Task {
        val accounts = wallet.privateKeyAccounts

        accounts.foreach { account =>
          val accountAddress = account.toAddress.toString

          if (!committedAccounts.contains(accountAddress)) {
            val fee          = 10000000L
            val wavesBalance = context.blockchain.balance(account.toAddress)

            if (wavesBalance < fee) {
              log.error(s"Insufficient balance for fee for account $accountAddress. Required: $fee, available: $wavesBalance")
            } else {
              if (wavesBalance < 100 * 100000000L) {
                log.warn(s"Balance is low for account $accountAddress: $wavesBalance. It's less than 100 WAVES.")
              }

              val timestamp = context.time.getTimestamp()

              val blsKP      = BlsKeyPair(account.privateKey)
              val blsMessage = blsKP.publicKey.arr ++ Ints.toByteArray(generationPeriodStart)
              val blsSig     = blsKP.sign(blsMessage)

              val commitToGenTxE = CommitToGenerationTransaction
                .create(
                  sender = account.publicKey,
                  endorserPublicKey = blsKP.publicKey,
                  generationPeriodStart = generationPeriodStart.next,
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
                      s"Failed to create transaction for account $accountAddress due to insufficient fee: ${TxValidationError.InsufficientFee}"
                    )
                  case e => log.error(s"Failed to create CommitToGenerationTransaction for account $accountAddress: $e")
                },
                tx => {
                  log.info(s"Created CommitToGenerationTransaction with id: ${tx.id()} for account $accountAddress")
                  context.broadcastTransaction(tx).resultE.fold(
                    err => {
                      if (err.toString.contains("already committed") || err.toString.contains("Expected the next period")) {
                        log.debug(s"Account $accountAddress already committed for period $generationPeriodStart")
                        committedAccounts.add(accountAddress)
                      } else {
                        log.warn(s"Failed to broadcast commitment for $accountAddress: $err")
                      }
                    },
                    {
                      case true =>
                        log.info(s"Successfully broadcasted commitment for $accountAddress for period $generationPeriodStart")
                        committedAccounts.add(accountAddress)
                      case false =>
                        log.debug(s"Failed to broadcast commitment for $accountAddress, maybe it already exists.")
                        committedAccounts.add(accountAddress)
                    }
                  )
                }
              )
            }
          } else {
            log.debug(s"Account $accountAddress already committed for current period")
          }
        }

        val newAccounts = accounts.map(_.toAddress.toString).toSet
        val removedAccounts = committedAccounts.filterNot(newAccounts.contains)
        if (removedAccounts.nonEmpty) {
          log.debug(s"Removing ${removedAccounts.size} accounts that are no longer in wallet from commitment tracking")
          committedAccounts --= removedAccounts
        }
      }

      def waitForNewHeight(currentHeight: Int): Task[Int] = {
        def check(): Task[Int] = {
          val newHeight = context.blockchain.height
          if (newHeight > currentHeight) {
            Task.now(newHeight)
          } else {
            Task.sleep(500.milli) >> check()
          }
        }
        check()
      }

      def heightLoop(h: Int): Task[Unit] = {
        waitForNewHeight(h).flatMap { newHeight =>
          val n = Math.floor((newHeight - 1).toDouble / generationPeriodLength).toLong
          val periodToCommit = (n + 1) * generationPeriodLength

          if (periodToCommit != currentPeriod) {
            log.info(s"New commitment period started: $periodToCommit (previous: $currentPeriod), resetting commitment tracking")
            committedAccounts.clear()
            currentPeriod = periodToCommit.toInt
          }

          log.debug(s"Checking for commitments at height $newHeight for period $periodToCommit. Tracked accounts: ${committedAccounts.size}")

          createTask(Height(periodToCommit.toInt)) >> heightLoop(newHeight)
        }
      }

      val initialHeight = context.blockchain.height
      if (initialHeight == 0) {
        waitForNewHeight(0).flatMap { firstRealHeight =>
          log.info(s"First block reached: $firstRealHeight, starting commitment loop")
          val n = Math.floor((firstRealHeight - 1).toDouble / generationPeriodLength).toLong
          currentPeriod = ((n + 1) * generationPeriodLength).toInt
          heightLoop(firstRealHeight)
        }.runAsyncLogErr
      } else {
        val n = Math.floor((initialHeight - 1).toDouble / generationPeriodLength).toLong
        currentPeriod = ((n + 1) * generationPeriodLength).toInt
        heightLoop(initialHeight).runAsyncLogErr
      }
    } else {
      log.warn("DeterministicFinality feature is not activated. CommitmentExtension will not start.")
    }
  }

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down CommitmentExtension")
    Future.successful(())
  }
}