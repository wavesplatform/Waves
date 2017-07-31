package scorex.block

import cats._
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.transaction.TransactionParser._
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{AssetAcc, _}
import scorex.utils.ScorexLogging

import scala.util.{Failure, Try}

case class Block(timestamp: Long, version: Byte, reference: ByteStr, signerData: SignerData,
                 consensusData: NxtLikeConsensusBlockData, transactionData: Seq[Transaction]) extends Signed {

  private lazy val versionField: ByteBlockField = ByteBlockField("version", version)
  private lazy val timestampField: LongBlockField = LongBlockField("timestamp", timestamp)
  private lazy val referenceField: BlockIdField = BlockIdField("reference", reference.arr)
  private lazy val signerDataField: SignerDataBlockField = SignerDataBlockField("signature", signerData)
  private lazy val consensusDataField = NxtConsensusBlockField(consensusData)
  private lazy val transactionDataField = TransactionsBlockField(version.toInt, transactionData)

  lazy val uniqueId: ByteStr = signerData.signature

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

  lazy val feesDistribution: Diff = Monoid[Diff].combineAll({
    val generator = signerData.generator
    val assetFees: Seq[(Option[AssetId], Long)] = transactionData.map(_.assetFee)
    assetFees
      .map { case (maybeAssetId, vol) => AssetAcc(generator, maybeAssetId) -> vol }
      .groupBy(a => a._1)
      .mapValues((records: Seq[(AssetAcc, Long)]) => records.map(_._2).sum)
  }.toList.map {
    case (AssetAcc(account, maybeAssetId), feeVolume) =>
      account -> (maybeAssetId match {
        case None => Portfolio(feeVolume, LeaseInfo.empty, Map.empty)
        case Some(assetId) => Portfolio(0L, LeaseInfo.empty, Map(assetId -> feeVolume))
      })
  }.map { case (acc, p) =>
    Diff.empty.copy(portfolios = Map(acc -> p))
  })

  override lazy val signatureValid: Boolean = EllipticCurveImpl.verify(signerData.signature.arr, bytesWithoutSignature, signerData.generator.publicKey)
  override lazy val signedDescendants: Seq[Signed] = transactionData
}


object Block extends ScorexLogging {
  type BlockIds = Seq[ByteStr]
  type BlockId = ByteStr

  val MaxTransactionsPerBlockVer1: Int = 100
  val MaxTransactionsPerBlockVer2: Int = 65535
  val BaseTargetLength: Int = 8
  val GeneratorSignatureLength: Int = 32

  val BlockIdLength = SignatureLength

  val TransactionSizeLength = 4

  def transParseBytes(version: Int,bytes: Array[Byte]): Try[Seq[Transaction]] = Try {
    if (bytes.isEmpty ) {
       Seq.empty
      } else {
        val v: (Array[Byte], Int) = version match {
        case 1 | 2 => (bytes.tail        , bytes.head) //  255  max
        case _ => ???
      }

      (1 to v._2).foldLeft((0: Int, Seq[Transaction]())) { case ((pos, txs), _) =>
          val transactionLengthBytes = v._1.slice(pos, pos + TransactionSizeLength)
          val transactionLength = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes = v._1.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
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

    val reference = ByteStr(bytes.slice(position, position + SignatureLength))
    position += SignatureLength

    val cBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val cBytes = bytes.slice(position, position + cBytesLength)
    val consData = NxtLikeConsensusBlockData(Longs.fromByteArray(cBytes.take(Block.BaseTargetLength)), cBytes.takeRight(Block.GeneratorSignatureLength))
    position += cBytesLength

    val tBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val tBytes = bytes.slice(position, position + tBytesLength)
    val txBlockField = transParseBytes(version.toInt, tBytes).get
    position += tBytesLength

    val genPK = bytes.slice(position, position + KeyLength)
    position += KeyLength

    val signature = ByteStr(bytes.slice(position, position + SignatureLength))

    Block(timestamp, version, reference, SignerData(PublicKeyAccount(genPK), signature), consData, txBlockField)
  }.recoverWith { case t: Throwable =>
    log.error("Error when parsing block", t)
    Failure(t)
  }

  def buildAndSign(version: Byte,
                   timestamp: Long,
                   reference: ByteStr,
                   consensusData: NxtLikeConsensusBlockData,
                   transactionData: Seq[Transaction],
                   signer: PrivateKeyAccount): Block = {
    val nonSignedBlock = Block(timestamp, version, reference, SignerData(signer, ByteStr.empty), consensusData, transactionData)
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(signer, toSign)
    require(reference.arr.length == SignatureLength, "Incorrect reference")
    require(consensusData.generationSignature.length == GeneratorSignatureLength, "Incorrect consensusData.generationSignature")
    require(signer.publicKey.length == KeyLength, "Incorrect signer.publicKey")
    nonSignedBlock.copy(signerData = SignerData(signer, ByteStr(signature)))
  }

  def genesisTransactions(gs: GenesisSettings): Seq[GenesisTransaction] = {
    gs.transactions.map { ts =>
      val acc = Address.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, gs.timestamp).right.get
    }
  }

  def genesis(genesisSettings: GenesisSettings): Either[ValidationError, Block] = {
    val version: Byte = 1

    val genesisSigner = PrivateKeyAccount(Array.empty)

    val transactionGenesisData = genesisTransactions(genesisSettings)
    val transactionGenesisDataField = TransactionsBlockFieldVersion1or2(transactionGenesisData)
    val consensusGenesisData = NxtLikeConsensusBlockData(genesisSettings.initialBaseTarget, Array.fill(DigestSize)(0: Byte))
    val consensusGenesisDataField = NxtConsensusBlockField(consensusGenesisData)
    val txBytesSize = transactionGenesisDataField.bytes.length
    val txBytes = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionGenesisDataField.bytes
    val cBytesSize = consensusGenesisDataField.bytes.length
    val cBytes = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ consensusGenesisDataField.bytes

    val reference = Array.fill(SignatureLength)(-1: Byte)

    val timestamp = genesisSettings.blockTimestamp
    val toSign: Array[Byte] = Array(version) ++
      Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0) ++
      reference ++
      cBytes ++
      txBytes ++
      genesisSigner.publicKey

    val signature = genesisSettings.signature.fold(EllipticCurveImpl.sign(genesisSigner, toSign))(_.arr)

    if (EllipticCurveImpl.verify(signature, toSign, genesisSigner.publicKey))
      Right(Block(timestamp = timestamp,
        version = version,
        reference = ByteStr(reference),
        signerData = SignerData(genesisSigner, ByteStr(signature)),
        consensusData = consensusGenesisData,
        transactionData = transactionGenesisData))
    else Left(GenericError("Passed genesis signature is not valid"))

  }
}
