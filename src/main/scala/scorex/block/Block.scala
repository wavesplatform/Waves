package scorex.block

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block.BlockId
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TypedTransaction._
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.util.{Failure, Try}

case class Block(timestamp: Long, version: Byte, reference: Block.BlockId, signerData: SignerData,
                 consensusData: NxtLikeConsensusBlockData, transactionData: Seq[Transaction]) {

  lazy val versionField: ByteBlockField = ByteBlockField("version", version)
  lazy val timestampField: LongBlockField = LongBlockField("timestamp", timestamp)
  lazy val referenceField: BlockIdField = BlockIdField("reference", reference)
  lazy val signerDataField: SignerDataBlockField = SignerDataBlockField("signature", signerData)
  lazy val uniqueId: BlockId = signerData.signature
  lazy val consensusDataField = NxtConsensusBlockField(consensusData)
  lazy val transactionDataField = TransactionsBlockField(transactionData)
  lazy val encodedId: String = Base58.encode(uniqueId)

  lazy val fee: Long =
    transactionData.map(_.assetFee)
      .map(a => AssetAcc(signerData.generator, a._1) -> a._2)
      .groupBy(a => a._1)
      .mapValues(_.map(_._2).sum)
      .values.sum

  lazy val json: JsObject =
    versionField.json ++
      timestampField.json ++
      referenceField.json ++
      consensusDataField.json ++
      transactionDataField.json ++
      signerDataField.json ++
      Json.obj(
        "fee" -> fee,
        "blocksize" -> bytes.length
      )

  lazy val bytes: Array[Byte] = {
    val txBytesSize = transactionDataField.bytes.length
    val txBytes = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionDataField.bytes

    val cBytesSize = consensusDataField.bytes.length
    val cBytes = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ consensusDataField.bytes

    versionField.bytes ++
      timestampField.bytes ++
      referenceField.bytes ++
      cBytes ++
      txBytes ++
      signerDataField.bytes
  }

  lazy val bytesWithoutSignature: Array[Byte] = bytes.dropRight(SignatureLength)

  lazy val blockScore: BigInt = (BigInt("18446744073709551616") / consensusData.baseTarget)
    .ensuring(_ > 0) // until we make smart-constructor validate consensusData.baseTarget to be positive


  override def equals(obj: scala.Any): Boolean = {
    import shapeless.syntax.typeable._
    obj.cast[Block].exists(_.uniqueId.sameElements(this.uniqueId))
  }
}


object Block extends ScorexLogging {
  type BlockId = Array[Byte]
  type BlockIds = Seq[BlockId]

  val BlockIdLength = SignatureLength

  def feesDistribution(block: Block): Map[AssetAcc, Long] = {
    val generator = block.signerDataField.value.generator
    val assetFees = block.transactionDataField.asInstanceOf[TransactionsBlockField].value.map(_.assetFee)
    assetFees.map(a => AssetAcc(generator, a._1) -> a._2).groupBy(a => a._1).mapValues(_.map(_._2).sum)
  }

  val TransactionSizeLength = 4

  def transParseBytes(bytes: Array[Byte]): Try[Seq[TypedTransaction]] = Try {
    bytes.isEmpty match {
      case true => Seq.empty
      case false =>
        val txData = bytes.tail
        val txCount = bytes.head // so 255 txs max
        (1 to txCount).foldLeft((0: Int, Seq[TypedTransaction]())) { case ((pos, txs), _) =>
          val transactionLengthBytes = txData.slice(pos, pos + TransactionSizeLength)
          val transactionLength = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes = txData.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
          val transaction = TypedTransaction.parseBytes(transactionBytes).get

          (pos + TransactionSizeLength + transactionLength, txs :+ transaction)
        }._2
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[Block] = Try {

    val version = bytes.head

    var position = 1

    val timestamp = Longs.fromByteArray(bytes.slice(position, position + 8))
    position += 8

    val reference = bytes.slice(position, position + Block.BlockIdLength)
    position += BlockIdLength

    val cBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val cBytes = bytes.slice(position, position + cBytesLength)
    val consData = NxtLikeConsensusBlockData(Longs.fromByteArray(cBytes.take(WavesConsensusModule.BaseTargetLength)), cBytes.takeRight(WavesConsensusModule.GeneratorSignatureLength))
    position += cBytesLength

    val tBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val tBytes = bytes.slice(position, position + tBytesLength)
    val txBlockField = transParseBytes(tBytes).get
    position += tBytesLength

    val genPK = bytes.slice(position, position + KeyLength)
    position += KeyLength

    val signature = bytes.slice(position, position + SignatureLength)

    new Block(timestamp, version, reference, SignerData(new PublicKeyAccount(genPK), signature), consData, txBlockField)
  }.recoverWith { case t: Throwable =>
    log.error("Error when parsing block", t)
    t.printStackTrace()
    Failure(t)
  }

  def build(version: Byte,
            timestamp: Long,
            reference: BlockId,
            consensusData: NxtLikeConsensusBlockData,
            transactionData: Seq[Transaction],
            generator: PublicKeyAccount,
            signature: Array[Byte])
  = new Block(timestamp, version, reference, SignerData(generator, signature), consensusData, transactionData)

  def buildAndSign(version: Byte,
                   timestamp: Long,
                   reference: BlockId,
                   consensusData: NxtLikeConsensusBlockData,
                   transactionData: Seq[Transaction],
                   signer: PrivateKeyAccount): Block = {
    val nonSignedBlock = build(version, timestamp, reference, consensusData, transactionData, signer, Array())
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(signer, toSign)
    build(version, timestamp, reference, consensusData, transactionData, signer, signature)
  }

  def genesis(concensusGenesisData: NxtLikeConsensusBlockData,
              transactionGenesisData: Seq[Transaction],
              timestamp: Long = 0L,
              signatureStringOpt: Option[String] = None): Block = {
    val version: Byte = 1

    val genesisSigner = new PrivateKeyAccount(Array.empty)

    val transactionGenesisDataField = TransactionsBlockField(transactionGenesisData)
    val concensusGenesisDataField = NxtConsensusBlockField(concensusGenesisData)
    val txBytesSize = transactionGenesisDataField.bytes.length
    val txBytes = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionGenesisDataField.bytes
    val cBytesSize = concensusGenesisDataField.bytes.length
    val cBytes = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ concensusGenesisDataField.bytes

    val reference = Array.fill(BlockIdLength)(-1: Byte)

    val toSign: Array[Byte] = Array(version) ++
      Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0) ++
      reference ++
      cBytes ++
      txBytes ++
      genesisSigner.publicKey

    val signature = signatureStringOpt.map(Base58.decode(_).get)
      .getOrElse(EllipticCurveImpl.sign(genesisSigner, toSign))

    require(EllipticCurveImpl.verify(signature, toSign, genesisSigner.publicKey), "Passed genesis signature is not valid")

    new Block(timestamp = timestamp,
      version = 1,
      reference = reference,
      signerData = SignerData(genesisSigner, signature),
      consensusData = concensusGenesisData,
      transactionData = transactionGenesisData)
  }
}
