package com.wavesplatform.network

import com.google.common.primitives.Ints
import com.google.protobuf.{ByteString, CodedOutputStream, WireFormat}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.{DigestLength, SignatureLength}
import com.wavesplatform.mining.MiningConstraints
import com.wavesplatform.protobuf.block.*
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{DataTransaction, Proofs, TxHelpers, TxVersion}

import java.io.ByteArrayOutputStream

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
        TxHelpers.removeScript(
          accountGen.sample.get,
          fee = 1L,
          version = TxVersion.V2
        )
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

  "GetBlockIdsSpec" - {
    def roundTrip(ids: Seq[ByteStr]) = GetBlockIdsSpec.deserializeData(GetBlockIdsSpec.serializeData(GetSignatures(ids)))

    "decodes a mix of 32- and 64-byte ids" in {
      val ids = Seq(bytes32gen.sample.get, bytes64gen.sample.get).map(ByteStr(_))
      roundTrip(ids).get.signatures shouldBe ids
    }

    "rejects ids of any other length" in {
      roundTrip(Seq(byteArrayGen(16).sample.get, byteArrayGen(48).sample.get).map(ByteStr(_))) should be a Symbol("failure")
      roundTrip(Seq(ByteStr.empty)) should be a Symbol("failure")
    }

    "rejects a declared count that does not match the entries" in {
      val bytes = Ints.toByteArray(2) ++ Array(DigestLength.toByte) ++ new Array[Byte](DigestLength)
      GetBlockIdsSpec.deserializeData(bytes) should be a Symbol("failure")
    }

    "rejects trailing bytes" in {
      val bytes = GetBlockIdsSpec.serializeData(GetSignatures(Seq(ByteStr(bytes32gen.sample.get)))) :+ 0.toByte
      GetBlockIdsSpec.deserializeData(bytes) should be a Symbol("failure")
    }

    "rejects more ids than a message may carry" in {
      val bytes =
        Ints.toByteArray(Int.MaxValue) ++ Array.fill(BlockIdSeqSpec.MaxIds + 1)(Array(DigestLength.toByte) ++ new Array[Byte](DigestLength)).flatten
      GetBlockIdsSpec.deserializeData(bytes) should be a Symbol("failure")
    }

    "decodes as many ids as fit the message" in {
      val ids = Seq.fill(BlockIdSeqSpec.MaxIds)(ByteStr(bytes32gen.sample.get))
      GetBlockIdsSpec.serializeData(GetSignatures(ids)).length should be <= GetBlockIdsSpec.maxLength
      roundTrip(ids).get.signatures shouldBe ids
    }
  }

  "GetSignaturesSpec rejects a count whose byte length overflows to match the payload" in {
    val k     = 2
    val bytes = Ints.toByteArray((1 << 26) + k) ++ new Array[Byte](k * SignatureLength) // (2^26 + k) * 64 wraps to k * 64
    GetSignaturesSpec.deserializeData(bytes) should be a Symbol("failure")
  }

  "GetSignaturesSpec decodes the most signatures a message may carry" in {
    val sigs = Seq.fill(200)(ByteStr(bytes64gen.sample.get))
    GetSignaturesSpec.deserializeData(GetSignaturesSpec.serializeData(GetSignatures(sigs))).get.signatures shouldBe sigs
  }

  "id-carrying requests reject ids of a wrong length" in {
    Seq(byteArrayGen(16).sample.get, byteArrayGen(SignatureLength + 1).sample.get).foreach { bytes =>
      MicroBlockRequestSpec.deserializeData(bytes) should be a Symbol("failure")
      MicroSnapshotRequestSpec.deserializeData(bytes) should be a Symbol("failure")
    }
  }
}
