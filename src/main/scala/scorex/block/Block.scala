package scorex.block

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}

import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser._
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.util.{Failure, Try}

case class Block(timestamp: Long, version: Byte, reference: ByteStr, signerData: SignerData,
                 consensusData: NxtLikeConsensusBlockData, transactionData: Seq[Transaction]) {

  private lazy val versionField: ByteBlockField = ByteBlockField("version", version)
  private lazy val timestampField: LongBlockField = LongBlockField("timestamp", timestamp)
  private lazy val referenceField: BlockIdField = BlockIdField("reference", reference.arr)
  private lazy val signerDataField: SignerDataBlockField = SignerDataBlockField("signature", signerData)
  private lazy val consensusDataField = NxtConsensusBlockField(consensusData)
  private lazy val transactionDataField = TransactionsBlockField(transactionData)

  lazy val uniqueId: ByteStr = signerData.signature
  lazy val encodedId: String = uniqueId.base58

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

  lazy val feesDistribution: Map[AssetAcc, Long] = {
    val generator = signerData.generator
    val assetFees: Seq[(Option[AssetId], Long)] = transactionData.map(_.assetFee)
    assetFees
      .map { case (maybeAssetId, vol) => AssetAcc(generator, maybeAssetId) -> vol }
      .groupBy(a => a._1)
      .mapValues((records: Seq[(AssetAcc, Long)]) => records.map(_._2).sum)
  }

  override def equals(obj: scala.Any): Boolean = {
    import shapeless.syntax.typeable._
    obj.cast[Block].exists(_.uniqueId == this.uniqueId)
  }
}


object Block extends ScorexLogging {
  type BlockIds = Seq[ByteStr]

  val MaxTransactionsPerBlock: Int = 100
  val BaseTargetLength: Int = 8
  val GeneratorSignatureLength: Int = 32

  val BlockIdLength = SignatureLength

  val TransactionSizeLength = 4

  def transParseBytes(bytes: Array[Byte]): Try[Seq[Transaction]] = Try {
    bytes.isEmpty match {
      case true => Seq.empty
      case false =>
        val txData = bytes.tail
        val txCount = bytes.head // so 255 txs max
        (1 to txCount).foldLeft((0: Int, Seq[Transaction]())) { case ((pos, txs), _) =>
          val transactionLengthBytes = txData.slice(pos, pos + TransactionSizeLength)
          val transactionLength = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes = txData.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
          val transaction = TransactionParser.parseBytes(transactionBytes).get

          (pos + TransactionSizeLength + transactionLength, txs :+ transaction)
        }._2
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[Block] = Try {

    val version = bytes.head

    var position = 1

    val timestamp = Longs.fromByteArray(bytes.slice(position, position + 8))
    position += 8

    val reference = ByteStr(bytes.slice(position, position + Block.BlockIdLength))
    position += BlockIdLength

    val cBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val cBytes = bytes.slice(position, position + cBytesLength)
    val consData = NxtLikeConsensusBlockData(Longs.fromByteArray(cBytes.take(Block.BaseTargetLength)), cBytes.takeRight(Block.GeneratorSignatureLength))
    position += cBytesLength

    val tBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val tBytes = bytes.slice(position, position + tBytesLength)
    val txBlockField = transParseBytes(tBytes).get
    position += tBytesLength

    val genPK = bytes.slice(position, position + KeyLength)
    position += KeyLength

    val signature =ByteStr(bytes.slice(position, position + SignatureLength))

    new Block(timestamp, version, reference, SignerData(PublicKeyAccount(genPK), signature), consData, txBlockField)
  }.recoverWith { case t: Throwable =>
    log.error("Error when parsing block", t)
    Failure(t)
  }

  def build(version: Byte,
            timestamp: Long,
            reference: ByteStr,
            consensusData: NxtLikeConsensusBlockData,
            transactionData: Seq[Transaction],
            generator: PublicKeyAccount,
            signature: Array[Byte])
  = new Block(timestamp, version, reference, SignerData(generator, ByteStr(signature)), consensusData, transactionData)

  def buildAndSign(version: Byte,
                   timestamp: Long,
                   reference: ByteStr,
                   consensusData: NxtLikeConsensusBlockData,
                   transactionData: Seq[Transaction],
                   signer: PrivateKeyAccount): Block = {
    val nonSignedBlock = build(version, timestamp, reference, consensusData, transactionData, signer, Array())
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(signer, toSign)
    build(version, timestamp, reference, consensusData, transactionData, signer, signature)
  }

  def genesis(consensusGenesisData: NxtLikeConsensusBlockData,
              transactionGenesisData: Seq[Transaction],
              timestamp: Long = 0L,
              signatureStringOpt: Option[String] = None): Block = {
    val version: Byte = 1

    val genesisSigner = PrivateKeyAccount(Array.empty)

    val transactionGenesisDataField = TransactionsBlockField(transactionGenesisData)
    val consensusGenesisDataField = NxtConsensusBlockField(consensusGenesisData)
    val txBytesSize = transactionGenesisDataField.bytes.length
    val txBytes = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionGenesisDataField.bytes
    val cBytesSize = consensusGenesisDataField.bytes.length
    val cBytes = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ consensusGenesisDataField.bytes

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
      reference = ByteStr(reference),
      signerData = SignerData(genesisSigner, ByteStr(signature)),
      consensusData = consensusGenesisData,
      transactionData = transactionGenesisData)
  }
}
