package scorex.block

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.joda.time.DateTime
import scorex.account.{Account, PublicKeyAccount}
import scorex.consensus.nxt.NxtBlockGenerationData
import scorex.consensus.qora.{QoraBlockGenerationData, QoraBlockGenerationDataParser}
import scorex.crypto.Crypto
import scorex.transaction.GenesisTransaction
import scorex.transaction.Transaction.ValidationResult
import settings.Constants
import settings.Constants.ConsensusAlgo.kernelData


object GenesisBlockParams {
  val version: Byte = 1
  val reference = Array[Byte](1, 1, 1, 1, 1, 1, 1, 1)
  val generator = new PublicKeyAccount(Array[Byte](1, 1, 1, 1, 1, 1, 1, 1))

  val ipoMembers = List(
    "2UyntBprhFgZPJ1tCtKBAwryiSnDSk9Xmh8",
    "Y2BXLjiAhPUMSo8iBbDEhv81VwKnytTXsH",
    "a8zcZPAj4HJjNLCmfRPVxurcB4REj8YNse",
    "hWUV4cjcGPgKjaNuRWAYyFMDZKadSPuwfP",
    "2etPX8BRivVTqr3vBvaCBeebhhGipbuzBNW",
    "dNKdbrqeykhxsnUpLjFTDHtTWHquiCcBGe",
    "5MkGmznxmA1Jm2F5KtxYVaf2Bfa6sy2XS1",
    "2Cqn5vN5iv7jDMehTiXTv3SGpxrCDAkAnBT",
    "2ihjht1NWTv2T8nKDMzx2RMmp7ZDEchXJus",
    "2kx3DyWJpYYfLErWpRMLHwkL1ZGyKHAPNKr"
  )

  def transactions(timestamp: Long) = ipoMembers.map { addr =>
    val recipient = new Account(addr)
    GenesisTransaction(recipient, 1000000000L, timestamp)
  }
}


abstract class GenesisBlock(override val generationData: kernelData, override val timestamp: Long)
  extends Block(version = GenesisBlockParams.version, reference = GenesisBlockParams.reference, timestamp,
    generator = GenesisBlockParams.generator, generationData,
    GenesisBlockParams.transactions(timestamp), Array.fill(64)(0)) {

  override def parent() = None

  override def isValid() = transactions.forall(_.isValid() == ValidationResult.VALIDATE_OKE)
}


object QoraGenesisBlockGenerationData {
  val generatingBalance = 10000000

  lazy val generatorSignature = {
    val versionBytes = Ints.toByteArray(GenesisBlockParams.version)
    val referenceBytes = Bytes.ensureCapacity(GenesisBlockParams.reference, 64, 0)
    val generatingBalanceBytes = Longs.toByteArray(generatingBalance)
    val generatorBytes = Bytes.ensureCapacity(GenesisBlockParams.generator.publicKey, 32, 0)

    val data = Bytes.concat(versionBytes, referenceBytes, generatingBalanceBytes, generatorBytes)
    val digest = Crypto.sha256(data)
    Bytes.concat(digest, digest)
  }.ensuring(sig => sig.size == QoraBlockGenerationDataParser.GENERATOR_SIGNATURE_LENGTH)

  lazy val generationData = new QoraBlockGenerationData(generatingBalance, generatorSignature)
}

object NxtGenesisBlockGenerationData {
  val InitialGenerationSignature = Array.fill(32)(0: Byte)
  //Nxt's initial base target * 10, as 10 bln tokens total instead of 1
  val InitialBaseTarget: Long = 153722867 * 10
  lazy val generationData = new NxtBlockGenerationData(NxtGenesisBlockGenerationData.InitialBaseTarget,
    NxtGenesisBlockGenerationData.InitialGenerationSignature)
}

object NxtGenesisBlock extends GenesisBlock(
  NxtGenesisBlockGenerationData.generationData.asInstanceOf[Constants.ConsensusAlgo.kernelData],
  new DateTime(2015, 5, 23, 10, 35).getMillis) {

  require(signature.length == 96)
}

object QoraGenesisBlock extends GenesisBlock(
  QoraGenesisBlockGenerationData.generationData.asInstanceOf[Constants.ConsensusAlgo.kernelData],
  new DateTime(2015, 5, 23, 10, 35).getMillis)