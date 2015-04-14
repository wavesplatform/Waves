package scorex.consensus.qora

import com.google.common.primitives.{Bytes, Longs}
import ntp.NTP
import scorex.account.PrivateKeyAccount
import scorex.block.{BlockStub, Block}
import scorex.crypto.Crypto
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.wallet.Wallet
import scala.collection.concurrent.TrieMap
import scala.util.Random


object QoraBlockGenerationFunctions {
    val RETARGET = 10
    val MIN_BALANCE = 1L
    val MAX_BALANCE = 10000000000L
    val MIN_BLOCK_TIME = 1 * 60
    val MAX_BLOCK_TIME = 5 * 60

    def generateBlock(): Option[Block] = {
      val blockStubs = Wallet.privateKeyAccounts().foldLeft(TrieMap[PrivateKeyAccount, BlockStub]()) { case (bm, account) =>
        if (account.generatingBalance >= BigDecimal(1)) {
          bm += account -> generateNextBlock(account, PrunableBlockchainStorage.lastBlock)
        }
        bm
      }

      val generators = blockStubs.keys.toIndexedSeq
      val randomGen = generators(Random.nextInt(generators.size))
      val blockStub = blockStubs(randomGen)

      if (blockStub.timestamp <= NTP.getTime) Some(Block(blockStub, randomGen)) else None
    }

    def getNextBlockGeneratingBalance(block: Block): Long = {
      if (block.height().get % RETARGET == 0) {
        //GET FIRST BLOCK OF TARGET
        val firstBlock = (1 to RETARGET - 1).foldLeft(block) { case (bl, _) => bl.parent().get }

        //CALCULATE THE GENERATING TIME FOR LAST 10 BLOCKS
        val generatingTime = block.timestamp - firstBlock.timestamp

        //CALCULATE EXPECTED FORGING TIME
        val expectedGeneratingTime = getBlockTime(block.generationData.generatingBalance) * RETARGET * 1000

        //CALCULATE MULTIPLIER
        val multiplier = expectedGeneratingTime / generatingTime.toDouble

        //CALCULATE NEW GENERATING BALANCE
        val generatingBalance = (block.generationData.generatingBalance * multiplier).toLong
        minMaxBalance(generatingBalance)
      } else block.generationData.generatingBalance
    }

    def getBaseTarget(generatingBalance: Long): Long = minMaxBalance(generatingBalance) * getBlockTime(generatingBalance)

    def getBlockTime(generatingBalance: Long): Long = {
      val percentageOfTotal = minMaxBalance(generatingBalance) / MAX_BALANCE.toDouble
      (MIN_BLOCK_TIME + ((MAX_BLOCK_TIME - MIN_BLOCK_TIME) * (1 - percentageOfTotal))).toLong
    }

    private def calculateSignature(solvingBlock: Block, account: PrivateKeyAccount) = {
      //PARENT GENERATOR SIGNATURE
      val generatorSignature = solvingBlock.generationData.generatorSignature

      val genBalanceBytes = Longs.toByteArray(getNextBlockGeneratingBalance(solvingBlock))

      require(account.publicKey.length == Block.GENERATOR_LENGTH)
      Crypto.sign(account, Bytes.concat(generatorSignature, genBalanceBytes, account.publicKey))
    }

    private def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block) = {
      require(account.generatingBalance > BigDecimal(0), "Zero generating balance in generateNextBlock")

      val signature = calculateSignature(lastBlock, account)
      val hash = Crypto.sha256(signature)
      val hashValue = BigInt(1, hash)

      //CALCULATE ACCOUNT TARGET
      val targetBytes = Array.fill(32)(Byte.MaxValue)
      val baseTarget = BigInt(getBaseTarget(getNextBlockGeneratingBalance(lastBlock)))
      //MULTIPLY TARGET BY USER BALANCE
      val target = BigInt(1, targetBytes) / baseTarget * account.generatingBalance.toBigInt()

      //CALCULATE GUESSES
      val guesses = hashValue / target + 1

      //CALCULATE TIMESTAMP
      val timestampRaw = guesses * 1000 + lastBlock.timestamp

      //CHECK IF NOT HIGHER THAN MAX LONG VALUE
      val timestamp = if (timestampRaw > Long.MaxValue) Long.MaxValue else timestampRaw.longValue()

      BlockStub(Block.Version, lastBlock.signature, timestamp, account,
        new QoraBlockGenerationData(getNextBlockGeneratingBalance(lastBlock),  signature))
    }

    private def minMaxBalance(generatingBalance: Long) =
      if (generatingBalance < MIN_BALANCE) MIN_BALANCE
      else if (generatingBalance > MAX_BALANCE) MAX_BALANCE
      else generatingBalance
}
