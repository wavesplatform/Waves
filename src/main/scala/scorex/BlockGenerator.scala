package scorex

import akka.actor.Actor
import com.google.common.primitives.{Bytes, Longs}
import controller.Controller
import database.{PrunableBlockchainStorage, UnconfirmedTransactionsDatabaseImpl}
import ntp.NTP
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scorex.crypto.Crypto
import scorex.transaction.Transaction
import scorex.transaction.Transaction.ValidationResult
import scorex.wallet.Wallet
import settings.Settings

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap

case object TryToGenerateBlock

class BlockGenerator extends Actor {

  import scorex.BlockGenerator._

  override def receive = {
    case TryToGenerateBlock =>
      if (!Controller.isUpToDate()) Controller.update()

      //CHECK IF WE HAVE CONNECTIONS
      if (Controller.getStatus == Controller.STATUS_OKE
        || (Controller.getStatus == Controller.STATUS_NO_CONNECTIONS && Settings.offlineGeneration)) {
        val blocks = TrieMap[PrivateKeyAccount, BlockStub]()

        //GENERATE NEW BLOCKS
        Wallet.privateKeyAccounts().foreach { account =>
          if (account.generatingBalance >= BigDecimal(1)) {
            //CHECK IF BLOCK FROM USER ALREADY EXISTS USE MAP ACCOUNT BLOCK EASY
            if (!blocks.containsKey(account)) {
              //GENERATE NEW BLOCK FOR USER
              blocks += account -> generateNextBlock(account, PrunableBlockchainStorage.lastBlock)
            }
          }
        }

        blocks.exists { case (account, blockStub) =>
          if (blockStub.timestamp <= NTP.getTime) {
            val block = formBlock(blockStub, account)
            if (block.transactions.nonEmpty) {
              println("Non-empty block: " + block)
            }
            Controller.newBlockGenerated(block)
          } else false
        }
      }
  }
}

object BlockGenerator {
  val RETARGET = 10
  val MIN_BALANCE = 1L
  val MAX_BALANCE = 10000000000L
  val MIN_BLOCK_TIME = 1 * 60
  val MAX_BLOCK_TIME = 5 * 60

  def getNextBlockGeneratingBalance(block: Block) = {
    if (block.height().get % RETARGET == 0) {
      //GET FIRST BLOCK OF TARGET
      val firstBlock = (1 to RETARGET - 1).foldLeft(block) { case (bl, _) => bl.parent().get}

      //CALCULATE THE GENERATING TIME FOR LAST 10 BLOCKS
      val generatingTime = block.timestamp - firstBlock.timestamp

      //CALCULATE EXPECTED FORGING TIME
      val expectedGeneratingTime = getBlockTime(block.generatingBalance) * RETARGET * 1000

      //CALCULATE MULTIPLIER
      val multiplier = expectedGeneratingTime / generatingTime.toDouble

      //CALCULATE NEW GENERATING BALANCE
      val generatingBalance = (block.generatingBalance * multiplier).toLong
      minMaxBalance(generatingBalance)
    } else block.generatingBalance
  }

  def getBaseTarget(generatingBalance: Long) = minMaxBalance(generatingBalance) * getBlockTime(generatingBalance)

  def getBlockTime(generatingBalance: Long) = {
    val percentageOfTotal = minMaxBalance(generatingBalance) / MAX_BALANCE.toDouble
    (MIN_BLOCK_TIME + ((MAX_BLOCK_TIME - MIN_BLOCK_TIME) * (1 - percentageOfTotal))).toLong
  }

  private[BlockGenerator] def generateNextBlock(account: PrivateKeyAccount, block: Block) = {
    require(account.generatingBalance > BigDecimal(0), "Zero generating balance in generateNextBlock")


    val signature = calculateSignature(block, account)
    val hash = Crypto.sha256(signature)
    val hashValue = BigInt(1, hash)

    //CALCULATE ACCOUNT TARGET
    val targetBytes = Array.fill(32)(Byte.MaxValue)
    val baseTarget = BigInt(getBaseTarget(getNextBlockGeneratingBalance(block)))
    //MULTIPLY TARGET BY USER BALANCE
    val target = BigInt(1, targetBytes) / baseTarget * account.generatingBalance.toBigInt()


    //CALCULATE GUESSES
    val guesses = hashValue / target + 1

    //CALCULATE TIMESTAMP
    val timestampRaw = guesses * 1000 + block.timestamp

    //CHECK IF NOT HIGHER THAN MAX LONG VALUE
    val timestamp = if (timestampRaw > Long.MaxValue) Long.MaxValue else timestampRaw.longValue()

    val version = 1
    BlockStub(version, block.signature, timestamp, getNextBlockGeneratingBalance(block), account, signature)
  }

  private def calculateSignature(solvingBlock: Block, account: PrivateKeyAccount) = {
    //WRITE PARENT GENERATOR SIGNATURE
    val generatorSignature = Bytes.ensureCapacity(solvingBlock.generatorSignature, Block.GENERATOR_SIGNATURE_LENGTH, 0)

    //WRITE GENERATING BALANCE
    val baseTargetBytesRaw = Longs.toByteArray(getNextBlockGeneratingBalance(solvingBlock))
    val baseTargetBytes = Bytes.ensureCapacity(baseTargetBytesRaw, Block.GENERATING_BALANCE_LENGTH, 0)

    //WRITE GENERATOR
    val generatorBytes = Bytes.ensureCapacity(account.publicKey, Block.GENERATOR_LENGTH, 0)

    //CALC SIGNATURE OF NEWBLOCKHEADER
    Crypto.sign(account, Bytes.concat(generatorSignature, baseTargetBytes, generatorBytes))
  }

  private def formBlock(stub: BlockStub, account: PrivateKeyAccount): Block = {

    //ORDER TRANSACTIONS BY FEE PER BYTE
    val orderedTransactions = UnconfirmedTransactionsDatabaseImpl.getAll().sortBy(_.feePerByte)

    /* warning: simplification here!
        QORA does break after first transaction matched conditions then repeat cycle
        (while orderedTransactions contains transactions to process)
     */
    val (_, transactions) = orderedTransactions.foldLeft((0, List[Transaction]())) {
      case ((totalBytes, filteredTxs), tx) =>
        if (tx.timestamp <= stub.timestamp && tx.deadline > stub.timestamp
          && tx.isValid() == ValidationResult.VALIDATE_OKE
          && totalBytes + tx.dataLength <= Block.MAX_TRANSACTION_BYTES) {

          (totalBytes + tx.dataLength, tx :: filteredTxs)
        } else (totalBytes, filteredTxs)
    }

    val data = transactions.foldLeft(stub.generatorSignature) { case (bytes, tx) =>
      Bytes.concat(bytes, tx.signature);
    }

    val transactionsSingature = Crypto.sign(account, data)

    Block(stub, transactions, transactionsSingature)
  }

  private def minMaxBalance(generatingBalance: Long) =
    if (generatingBalance < MIN_BALANCE) MIN_BALANCE
    else if (generatingBalance > MAX_BALANCE) MAX_BALANCE
    else generatingBalance
}