package scorex.lagonaki.unit

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.produce
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction._
import org.scalatest.prop.PropertyChecks


class BlockSpecification extends PropSpec with PropertyChecks with TransactionGen with Matchers {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val time = System.currentTimeMillis() - 5000

  val blockGen = for {
    baseTarget <- arbitrary[Long]
    reference <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
    generationSignature <- byteArrayGen(Block.GeneratorSignatureLength)
    assetBytes <- byteArrayGen(AssetIdLength)
    assetId = Some(ByteStr(assetBytes))
    sender <- accountGen
    recipient <- accountGen
    paymentTransaction <- paymentGeneratorP(time, sender, recipient)
    transferTrancation <- transferGeneratorP(1 + time, sender, recipient, assetId, None)
    anotherPaymentTransaction <- paymentGeneratorP(2 + time, sender, recipient)
    transactionData = Seq(paymentTransaction, transferTrancation, anotherPaymentTransaction)

  } yield (baseTarget, reference, generationSignature, recipient, transactionData)


  property(" block with txs bytes/parse roundtrip version 1,2") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen){
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          val block = Block.buildAndSign(version, time, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), transactionData, recipient, Set.empty).explicitGet()
          val parsedBlock = Block.parseBytes(block.bytes).get
          assert(Signed.validateSignatures(block).isRight)
          assert(Signed.validateSignatures(parsedBlock).isRight)
          assert(parsedBlock.consensusData.generationSignature.sameElements(generationSignature))
          assert(parsedBlock.version.toInt == version)
          assert(parsedBlock.signerData.generator.publicKey.sameElements(recipient.publicKey))
      }
    }
  }

  property(" block version 1,2 could not contain supported feature flags") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen){
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          Block.buildAndSign(
            version,
            time,
            reference,
            NxtLikeConsensusBlockData(baseTarget, generationSignature),
            transactionData,
            recipient,
            Set(1)) should produce("could not contain supported feature flags")
      }
    }
  }

  property(s" feature flags limit is ${Block.MaxFeaturesInBlock}") {
    val version = 3.toByte
    val supportedFeatures = (0 to Block.MaxFeaturesInBlock * 2).map(_.toShort).toSet

    forAll(blockGen){
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        Block.buildAndSign(
          version,
          time,
          reference,
          NxtLikeConsensusBlockData(baseTarget, generationSignature),
          transactionData,
          recipient,
          supportedFeatures) should produce(s"Block could not contain more than ${Block.MaxFeaturesInBlock} feature flags")
    }
  }

  property(" block with txs bytes/parse roundtrip version 3") {
    val version = 3.toByte
    val featuresCount = Gen.choose(0,Block.MaxFeaturesInBlock).sample.get
    val supportedFeatures = Gen.containerOfN[Set, Short](featuresCount, arbitrary[Short]).sample.get

    forAll(blockGen){
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        val block = Block.buildAndSign(version, time, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), transactionData, recipient, supportedFeatures).explicitGet()
        val parsedBlock = Block.parseBytes(block.bytes).get
        assert(Signed.validateSignatures(block).isRight)
        assert(Signed.validateSignatures(parsedBlock).isRight)
        assert(parsedBlock.consensusData.generationSignature.sameElements(generationSignature))
        assert(parsedBlock.version.toInt == version)
        assert(parsedBlock.signerData.generator.publicKey.sameElements(recipient.publicKey))
        assert(parsedBlock.supportedFeaturesIds == supportedFeatures)
    }
  }
}