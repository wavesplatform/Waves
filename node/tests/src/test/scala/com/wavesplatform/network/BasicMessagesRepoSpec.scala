package com.wavesplatform.network

import java.io.ByteArrayOutputStream

import com.google.protobuf.{ByteString, CodedOutputStream, WireFormat}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.mining.MiningConstraints
import com.wavesplatform.protobuf.block._
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.{DataTransaction, Proofs, TxVersion}

class BasicMessagesRepoSpec extends FreeSpec {
  "PBBlockSpec max length" in {
    val maxSizedHeader = PBBlock.Header(
      Byte.MaxValue,
      ByteString.copyFrom(bytes64gen.sample.get),
      Long.MaxValue,
      ByteString.copyFrom(byteArrayGen(VanillaBlock.GenerationVRFSignatureLength).sample.get),
      Seq.fill(VanillaBlock.MaxFeaturesInBlock)(Short.MaxValue),
      Long.MaxValue,
      Byte.MaxValue,
      ByteString.copyFrom(bytes32gen.sample.get),
      Long.MaxValue,
      ByteString.copyFrom(bytes32gen.sample.get)
    )
    val maxSignature = ByteString.copyFrom(bytes64gen.sample.get)

    val headerSize    = maxSizedHeader.serializedSize
    val signatureSize = maxSignature.toByteArray.length

    val headerPBPrefix      = new ByteArrayOutputStream()
    val codedHeaderPBPrefix = CodedOutputStream.newInstance(headerPBPrefix)
    codedHeaderPBPrefix.writeTag(PBBlock.HEADER_FIELD_NUMBER, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    codedHeaderPBPrefix.writeUInt32NoTag(headerSize)
    codedHeaderPBPrefix.flush()

    val signaturePBPrefix      = new ByteArrayOutputStream()
    val codedSignaturePBPrefix = CodedOutputStream.newInstance(signaturePBPrefix)
    codedSignaturePBPrefix.writeTag(PBBlock.SIGNATURE_FIELD_NUMBER, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    codedSignaturePBPrefix.writeUInt32NoTag(maxSignature.toByteArray.length)
    codedSignaturePBPrefix.flush()

    val transactionPBPrefix               = new ByteArrayOutputStream()
    val codedTransactionMaxLengthPBPrefix = CodedOutputStream.newInstance(transactionPBPrefix)
    codedTransactionMaxLengthPBPrefix.writeTag(PBBlock.TRANSACTIONS_FIELD_NUMBER, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    codedTransactionMaxLengthPBPrefix.writeUInt32NoTag(MiningConstraints.MaxTxsSizeInBytes)
    codedTransactionMaxLengthPBPrefix.flush()

    val minPossibleTransactionSize = PBTransactions
      .protobuf(
        SetScriptTransaction
          .selfSigned(
            TxVersion.V2,
            accountGen.sample.get,
            None,
            1L,
            0L
          )
          .explicitGet()
      )
      .serializedSize

    val maxSize =
      headerPBPrefix.toByteArray.length + headerSize +
        signaturePBPrefix.toByteArray.length + signatureSize +
        MiningConstraints.MaxTxsSizeInBytes +
        (transactionPBPrefix.toByteArray.length * MiningConstraints.MaxTxsSizeInBytes / minPossibleTransactionSize)

    maxSize should be <= PBBlockSpec.maxLength
  }

  "PBTransactionSpec max length" in {
    val maxSizeTransaction = PBSignedTransaction(
      PBSignedTransaction.Transaction.WavesTransaction(
        PBTransaction(
          Byte.MaxValue,
          ByteString.copyFrom(bytes32gen.sample.get),
          Some(PBAmounts.fromAssetAndAmount(IssuedAsset(ByteStr(bytes32gen.sample.get)), Long.MaxValue)),
          Long.MaxValue,
          Byte.MaxValue
        )
      ),
      Seq.fill(Proofs.MaxProofs)(ByteString.copyFrom(byteArrayGen(Proofs.MaxProofSize).sample.get))
    )

    val dataPBPrefix      = new ByteArrayOutputStream()
    val codedDataPBPrefix = CodedOutputStream.newInstance(dataPBPrefix)
    codedDataPBPrefix.writeTag(Transaction.DATA_TRANSACTION_FIELD_NUMBER, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    codedDataPBPrefix.writeUInt32NoTag(DataTransaction.MaxProtoBytes)
    codedDataPBPrefix.flush()

    val size = maxSizeTransaction.serializedSize + dataPBPrefix.toByteArray.length + DataTransaction.MaxProtoBytes

    size should be <= PBTransactionSpec.maxLength
  }
}
