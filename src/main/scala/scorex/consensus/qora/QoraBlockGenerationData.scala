package scorex.consensus.qora

import java.util

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.Json
import scorex.block.{QoraGenesisBlockGenerationData, Block, QoraGenesisBlock}
import scorex.consensus.{BlockGenerationDataParser, BlockGenerationData}
import scorex.crypto.{Crypto, Base58}
import scorex.database.blockchain.PrunableBlockchainStorage
import settings.Constants


class QoraBlockGenerationData(val generatingBalance: Long, val generatorSignature: Array[Byte])
  extends BlockGenerationData {

  import QoraBlockGenerationDataParser._

  require(generatingBalance > 0)

  override def toBytes: Array[Byte] = Bytes.concat(
    Longs.toByteArray(generatingBalance),
    generatorSignature
  ).ensuring(_.length == GENERATION_DATA_LENGTH)

  override def toJson = Json.obj(
    "generatingBalance" -> generatingBalance,
    "generatorSignature" -> Base58.encode(generatorSignature)
  )

  override def signature() = generatorSignature

  override def isGenesis = QoraGenesisBlockGenerationData.generatorSignature.sameElements(generatorSignature)

  override def isValid(block: Block): Boolean = {
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

  override def isSignatureValid(block:Block):Boolean = {
    val generatingBalanceBytes = Longs.toByteArray(generatingBalance).ensuring(_.size == GENERATING_BALANCE_LENGTH)

    val blockSignature = Bytes.concat(util.Arrays.copyOfRange(block.reference, 0, GENERATOR_SIGNATURE_LENGTH),
      generatingBalanceBytes,
      block.generator.publicKey)

    Crypto.verify(signature(), blockSignature, block.generator.publicKey)
  }

  override def blockScore() = BigInt(1)
}