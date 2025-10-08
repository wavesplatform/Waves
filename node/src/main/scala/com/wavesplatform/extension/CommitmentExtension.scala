package com.wavesplatform.extension

import com.google.common.primitives.Ints
import com.wavesplatform.crypto
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.extensions.{Context, Extension}
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
    val settings = context.settings
    val generationPeriodLength = settings.blockchainSettings.functionalitySettings.generationPeriodLength
    val wallet = context.wallet

    def loop(): Task[Unit] = {
      val currentHeight = context.blockchain.height
      val currentPeriodN = Math.floor((currentHeight - 1).toDouble / generationPeriodLength).toLong
      val nextPeriodStart = (currentPeriodN + 1) * generationPeriodLength + 1

      val waitHeight = nextPeriodStart
      val waitTime = (waitHeight - currentHeight) * settings.blockchainSettings.genesis.averageBlockDelay.toMillis
      log.info(s"Current height: $currentHeight, next generation period starts at: $nextPeriodStart, waiting for ${waitTime}ms")

      for {
        _ <- Task.sleep(if (waitTime > 0) waitTime.millis else 0.millis)
        _ <- createTask(Height(nextPeriodStart.toInt))
        _ <- loop()
      } yield ()
    }

    def createTask(generationPeriodStart: Height): Task[Unit] = Task {
      wallet.privateKeyAccounts.headOption.foreach { account =>
        val fee = 100000L
        val wavesBalance = context.blockchain.balance(account.toAddress)

        if (wavesBalance < fee) {
          log.error(s"Insufficient balance for fee. Required: $fee, available: $wavesBalance")
        } else {
          if (wavesBalance < 100 * 100000000L) {
            log.warn(s"Balance is low: $wavesBalance. It's less than 100 WAVES.")
          }

          val timestamp = context.time.getTimestamp()

          val blsKP = BlsKeyPair(account.privateKey)
          val blsMessage = blsKP.publicKey.arr ++ Ints.toByteArray(generationPeriodStart)
          val blsSig = blsKP.sign(blsMessage)

          val commitToGenTxE = CommitToGenerationTransaction.create(
            sender = account.publicKey,
            endorserPublicKey = blsKP.publicKey,
            generationPeriodStart = generationPeriodStart,
            timestamp = timestamp,
            feeInWaves = fee,
            commitmentSignature = blsSig,
            proofs = Proofs.empty,
            chainId = settings.blockchainSettings.addressSchemeCharacter.toByte
          ).map(tx => tx.copy(proofs = Proofs(crypto.sign(account.privateKey, tx.bodyBytes()))))

          commitToGenTxE.fold(
            { 
              case e: TxValidationError.InsufficientFee => log.error(s"Failed to create transaction due to insufficient fee: $e")
              case e => log.error(s"Failed to create CommitToGenerationTransaction: $e")
            },
            tx => {
              log.info(s"Created CommitToGenerationTransaction with id: ${tx.id()}")
              log.info(s"BLS Public Key: ${blsKP.publicKey.base58}")
              log.info(s"Commitment Signature: ${blsSig.base58}")
              context.broadcastTransaction(tx)
            }
          )
        }
      }
    }

    loop().runAsyncLogErr
  }

  override def shutdown(): Future[Unit] = {
    log.info("Shutting down CommitmentExtension")
    Future.successful(())
  }
}
