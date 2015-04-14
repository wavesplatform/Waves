package scorex.consensus.qora

import java.util

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.Json
import scorex.block.{Block, GenesisBlockParams}
import scorex.crypto.{Crypto, Base58}
import scorex.database.blockchain.PrunableBlockchainStorage


class QoraBlockGenerationData(val generatingBalance: Long,
                          val generatorSignature: Array[Byte]) {
  import QoraBlockGenerationData._
  require(generatingBalance > 0)

  def toBytes: Array[Byte] = Bytes.concat(
    Longs.toByteArray(generatingBalance),
    generatorSignature
  ).ensuring(_.length == GENERATION_DATA_LENGTH)

  def toJson = Json.obj(
    "generatingBalance" -> generatingBalance,
    "generatorSignature" -> Base58.encode(generatorSignature)
  )

  def signature() = generatorSignature

  def isGenesis = GenesisBlockParams.generatorSignature.sameElements(generatorSignature)

  def isValid(block: Block): Boolean = {
    if (generatingBalance != QoraBlockGenerationFunctions.getNextBlockGeneratingBalance(block.parent().get)) {
      //CHECK IF GENERATING BALANCE IS CORRECT
      false
    } else {
      //CREATE TARGET
      val targetBytes = Array.fill(32)(Byte.MaxValue)

      //DIVIDE TARGET BY BASE TARGET
      val baseTarget = BigInt(QoraBlockGenerationFunctions.getBaseTarget(generatingBalance))
      val genBalance = PrunableBlockchainStorage.generationBalance(block.generator.address).toBigInt()
      val target0 = BigInt(1, targetBytes) / baseTarget * genBalance

      //MULTIPLE TARGET BY GUESSES
      val guesses = (block.timestamp - block.parent().get.timestamp) / 1000
      val lowerTarget = target0 * BigInt(guesses - 1)
      val target = target0 * BigInt(guesses)

      //CONVERT HIT TO BIGINT
      val hit = BigInt(1, Crypto.sha256(generatorSignature))

      if (hit >= target) {
        false
      } else if (hit < lowerTarget) {
        false
      } else true
    }
  }

  def isSignatureValid(block:Block):Boolean = {
    val generatingBalanceBytes = Longs.toByteArray(generatingBalance).ensuring(_.size == GENERATING_BALANCE_LENGTH)

    val blockSignature = Bytes.concat(util.Arrays.copyOfRange(block.reference, 0, GENERATOR_SIGNATURE_LENGTH),
      generatingBalanceBytes,
      block.generator.publicKey)

    Crypto.verify(signature(), blockSignature, block.generator.publicKey)
  }
}


object QoraBlockGenerationData {
  val GENERATING_BALANCE_LENGTH = 8
  val GENERATOR_SIGNATURE_LENGTH = 64
  val GENERATION_DATA_LENGTH = GENERATING_BALANCE_LENGTH + GENERATOR_SIGNATURE_LENGTH

  def parse(bytes:Array[Byte]):QoraBlockGenerationData = {
    val generatingBalance = Longs.fromByteArray(bytes.take(GENERATING_BALANCE_LENGTH))
    val generatorSignature = bytes.drop(GENERATING_BALANCE_LENGTH)
    new QoraBlockGenerationData(generatingBalance, generatorSignature)
  }
}