package com.wavesplatform.lagonaki.unit

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.crypto
import com.wavesplatform.test._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

class BlockSpecification extends PropSpec {

  val time = System.currentTimeMillis() - 5000

  val blockGen = for {
    baseTarget          <- arbitrary[Long]
    reference           <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
    generationSignature <- byteArrayGen(Block.GenerationSignatureLength)
    assetBytes          <- byteArrayGen(AssetIdLength)
    assetId = IssuedAsset(ByteStr(assetBytes))
    sender                    <- accountGen
    recipient                 <- accountGen
    paymentTransaction        <- wavesTransferGeneratorP(time, sender, recipient.toAddress)
    transferTrancation        <- transferGeneratorP(1 + time, sender, recipient.toAddress, assetId, Waves)
    anotherPaymentTransaction <- wavesTransferGeneratorP(2 + time, sender, recipient.toAddress)
    transactionData = Seq(paymentTransaction, transferTrancation, anotherPaymentTransaction)
  } yield (baseTarget, reference, ByteStr(generationSignature), recipient, transactionData)

  def bigBlockGen(amt: Int): Gen[Block] =
    for {
      baseTarget          <- arbitrary[Long]
      reference           <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
      generationSignature <- byteArrayGen(Block.GenerationSignatureLength)
      sender                                  <- accountGen
      recipient                               <- accountGen
      paymentTransaction: TransferTransaction <- wavesTransferGeneratorP(time, sender, recipient.toAddress)
    } yield Block
      .buildAndSign(
        3.toByte,
        time,
        reference,
        baseTarget,
        ByteStr(generationSignature),
        Seq.fill(amt)(paymentTransaction),
        recipient,
        Seq.empty,
        -1L
      )
      .explicitGet()

  property(" block with txs bytes/parse roundtrip version 1,2") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen) {
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          val block = Block
            .buildAndSign(
              version,
              time,
              reference,
              baseTarget,
              generationSignature,
              transactionData,
              recipient,
              Seq.empty,
              -1L
            )
            .explicitGet()
          val parsedBlock = Block.parseBytes(block.bytes()).get
          assert(block.signatureValid())
          assert(parsedBlock.signatureValid())
          assert(parsedBlock.header.generationSignature == generationSignature)
          assert(parsedBlock.header.version.toInt == version)
          assert(parsedBlock.header.generator == recipient.publicKey)
      }
    }
  }

  property(" block version 1,2 could not contain feature votes") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen) {
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          Block.buildAndSign(
            version,
            time,
            reference,
            baseTarget,
            generationSignature,
            transactionData,
            recipient,
            Seq(1),
            -1L
          ) should produce("could not contain feature votes")
      }
    }
  }

  property(s" feature flags limit is ${Block.MaxFeaturesInBlock}") {
    val version           = 3.toByte
    val supportedFeatures = (0 to Block.MaxFeaturesInBlock * 2).map(_.toShort)

    forAll(blockGen) {
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        Block.buildAndSign(
          version,
          time,
          reference,
          baseTarget,
          generationSignature,
          transactionData,
          recipient,
          supportedFeatures,
          -1L
        ) should produce(s"Block could not contain more than ${Block.MaxFeaturesInBlock} feature votes")
    }
  }
  property(" block with txs bytes/parse roundtrip version 3") {
    val version = 3.toByte

    val faetureSetGen: Gen[Seq[Short]] = Gen.choose(0, Block.MaxFeaturesInBlock).flatMap(fc => Gen.listOfN(fc, arbitrary[Short])).map(_.distinct)

    forAll(blockGen, faetureSetGen) {
      case ((baseTarget, reference, generationSignature, recipient, transactionData), featureVotes) =>
        val block = Block
          .buildAndSign(
            version,
            time,
            reference,
            baseTarget,
            generationSignature,
            transactionData,
            recipient,
            featureVotes,
            -1L
          )
          .explicitGet()
        val parsedBlock = Block.parseBytes(block.bytes()).get
        assert(block.signatureValid())
        assert(parsedBlock.signatureValid())
        assert(parsedBlock.header.generationSignature == generationSignature)
        assert(parsedBlock.header.version.toInt == version)
        assert(parsedBlock.header.generator == recipient.publicKey)
        assert(parsedBlock.header.featureVotes == featureVotes)
    }
  }

  property("block signed by a weak public key is invalid") {
    val weakAccount = PublicKey(Array.fill(32)(0: Byte))
    forAll(blockGen) {
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        val block = Block
          .create(
            3.toByte,
            time,
            reference,
            baseTarget,
            generationSignature,
            weakAccount,
            Seq.empty,
            -1L,
            transactionData
          )
          .copy(signature = ByteStr(Array.fill(64)(0: Byte)))
        block.signatureValid() shouldBe false
    }
  }

  ignore("sign time for 60k txs") {
    forAll(randomTransactionsGen(60000), accountGen, byteArrayGen(Block.BlockIdLength), byteArrayGen(Block.GenerationSignatureLength)) {
      case ((txs, acc, ref, gs)) =>
        val (block, t0) =
          Instrumented.withTimeMillis(
            Block.buildAndSign(3.toByte, 1, ByteStr(ref), 1, ByteStr(gs), txs, acc, Seq.empty, -1L).explicitGet()
          )
        val (bytes, t1) = Instrumented.withTimeMillis(block.bytes().dropRight(crypto.SignatureLength))
        val (hash, t2)  = Instrumented.withTimeMillis(crypto.fastHash(bytes))
        val (sig, t3)   = Instrumented.withTimeMillis(crypto.sign(acc.privateKey, hash))
    }
  }

  ignore("serialize and deserialize big block") {
    forAll(bigBlockGen(100 * 1000)) {
      case block =>
        val parsedBlock = Block.parseBytes(block.bytes()).get
        block.signatureValid() shouldBe true
        parsedBlock.signatureValid() shouldBe true
    }
  }
}
