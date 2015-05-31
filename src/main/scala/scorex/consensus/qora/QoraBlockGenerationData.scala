package scorex.consensus.qora

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.Json
import scorex.block.{Block, QoraGenesisBlockGenerationData}
import scorex.consensus.BlockGenerationData
import scorex.crypto.{Base58, Crypto}
import scorex.database.blockchain.PrunableBlockchainStorage


case class QoraBlockGenerationData(generatingBalance: Long, generatorSignature: Array[Byte])
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

  override def isValid(block: Block): Boolean =
    if (generatingBalance != QoraBlockGenerationFunctions.getNextBlockGeneratingBalance(block.parent().get)) {
      //CHECK IF GENERATING BALANCE IS CORRECT
      false
    } else {
      //target base
      val targetBytes = Array.fill(32)(Byte.MaxValue)
      val baseTarget = BigInt(QoraBlockGenerationFunctions.getBaseTarget(generatingBalance))
      val genBalance = PrunableBlockchainStorage.generationBalance(block.generator.address).toBigInt()
      val target0 = BigInt(1, targetBytes) / baseTarget * genBalance

      //target bounds
      val guesses = (block.timestamp - block.parent().get.timestamp) / 1000
      val lowerTarget = target0 * (guesses - 1)
      val target = target0 * guesses

      val hit = BigInt(1, Crypto.sha256(generatorSignature))

      //generation check
      hit >= lowerTarget && hit < target
    }


  override def isSignatureValid(block: Block): Boolean = {
    val generatingBalanceBytes = Longs.toByteArray(generatingBalance).ensuring(_.size == GENERATING_BALANCE_LENGTH)

    val blockSignatureImage = Bytes.concat(block.reference.take(GENERATOR_SIGNATURE_LENGTH),
      generatingBalanceBytes,
      block.generator.publicKey)

    Crypto.verify(signature(), blockSignatureImage, block.generator.publicKey)
  }

  override def blockScore() = BigInt(1)
}