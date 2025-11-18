package com.wavesplatform.lagonaki.unit

import com.google.protobuf.CodedInputStream
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.serialization.MicroBlockSerializer
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.mining.Miner
import com.wavesplatform.protobuf.block.{PBFinalizationVoting, PBFinalizationVotings}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.protobuf.transaction.{PBTransactions, PBSignedTransaction}
import com.wavesplatform.state.{GeneratorIndex, Height}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.*
import org.scalamock.scalatest.MockFactory

import scala.util.Random

class MicroBlockSpecification extends FunSuite with MockFactory {

  private val prevResBlockSig  = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  private val totalResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  private val stateHash        = ByteStr.fill(DigestLength)(Random.nextInt(100).toByte)
  private val reference        = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  private val sender           = KeyPair(reference.dropRight(2))
  private val gen              = KeyPair(reference)

  test("MicroBlock with txs bytes/parse roundtrip, without finalizationVoting") {

    val ts = System.currentTimeMillis() - 5000
    val tr: TransferTransaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, Waves, 5, Waves, 2, ByteStr.empty, ts + 1).explicitGet()
    val assetId = IssuedAsset(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, assetId, 5, Waves, 2, ByteStr.empty, ts + 2).explicitGet()

    val transactions = Seq(tr, tr2)

    val microBlock  = MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, Some(stateHash), None).explicitGet()
    val parsedBlock = MicroBlock.parseBytes(MicroBlockSerializer.toBytes(microBlock)).get

    assert(microBlock.signaturesValid().isRight)
    assert(parsedBlock.signaturesValid().isRight)

    assert(microBlock.signature == parsedBlock.signature)
    assert(microBlock.sender == parsedBlock.sender)
    assert(microBlock.totalResBlockSig == parsedBlock.totalResBlockSig)
    assert(microBlock.reference == parsedBlock.reference)
    assert(microBlock.transactionData == parsedBlock.transactionData)
    assert(microBlock.stateHash == parsedBlock.stateHash)
    assert(microBlock == parsedBlock)
  }

  test("MicroBlock with txs bytes/parse roundtrip, with finalizationVoting") {
    val ts = System.currentTimeMillis() - 5000
    val tr: TransferTransaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, Waves, 5, Waves, 2, ByteStr.empty, ts + 1).explicitGet()
    val assetId = IssuedAsset(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, assetId, 5, Waves, 2, ByteStr.empty, ts + 2).explicitGet()

    val transactions = Seq(tr, tr2)

    val aggregatedEndorsement = BlsSignature.NonEmpty(Array.fill(BlsSignature.SizeInBytes)(1.toByte))

    val finalizedHeight = Height(5)
    val finalizedId     = ByteStr(Array.fill(Block.BlockIdLength)(2.toByte))
    val endorsedId      = ByteStr(Array.fill(Block.BlockIdLength)(3.toByte))
    val blsEndorser     = BlsKeyPair(TxHelpers.signer(7).privateKey)
    val conflictEndorsements: IndexedSeq[BlockEndorsement] =
      IndexedSeq(BlockEndorsement.signed(blsEndorser, GeneratorIndex(7), finalizedId, finalizedHeight, endorsedId))

    val finalizationVoting = Some(
      FinalizationVoting(
        valid = Seq(GeneratorIndex(1), GeneratorIndex(2), GeneratorIndex(3)),
        finalizedHeight = finalizedHeight.toInt,
        aggregatedEndorsement = aggregatedEndorsement,
        conflict = conflictEndorsements
      )
    )

    val microBlock =
      MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, Some(stateHash), finalizationVoting).explicitGet()
    val parsedBlock = MicroBlock.parseBytes(MicroBlockSerializer.toBytes(microBlock)).get

    assert(microBlock.signaturesValid().isRight)
    assert(parsedBlock.signaturesValid().isRight)

    assert(microBlock.signature == parsedBlock.signature)
    assert(microBlock.sender == parsedBlock.sender)
    assert(microBlock.totalResBlockSig == parsedBlock.totalResBlockSig)
    assert(microBlock.reference == parsedBlock.reference)
    assert(microBlock.transactionData == parsedBlock.transactionData)
    assert(microBlock.stateHash == parsedBlock.stateHash)
    assert(microBlock.finalizationVoting == parsedBlock.finalizationVoting)
    assert(microBlock == parsedBlock)

  }


  test("Go FinalizationVoting parsed successfully") {
    val goString =
      "CgMBAgMSYIMo5F9oE9mJs6Kk/oAmO84HcXie+UmvhLWI0Muqnw3yCi5yekgkQgvH7A/AvPsAIhZneJFnHEX1/KZP9TxYFIxmbX5hcCECeRuKVXscQ1EZLvM+Hr13LHuuL1dly8W+ixrrAQgBEkBpxb4/AhKzhetm0OirYRJGCyY8B3xEfe5k8p5MnRx3OP7JzJFk/gUjXZ4pbUbVtuKfNhmGchlmxT3RNQEZ0YCAGLlgIkDVwvFq3zo0CKVUNrgbDbDy+ROY88ZTY/KfNW7693dcDyhYxOKyXOAEl1eT2pZyBB7k/mAeXwKUnXx7+pUTFOeDKmBGvXB/FKFQiVKk6CpaNmqoerGF2G/U8xmGKYdXA67G3dyA2VqjRKtIJa27xHSsSKFvtch7FrMyokkDABL8a6bH8nYej4RjrxGA5Qd2Gb+PVYZo/Fq/GTZ1PAh6r9EY59M="
    val res = PBFinalizationVoting.parseFrom(CodedInputStream.newInstance(Base64.decode(goString)))
    println(s"res: $res}")
  }

  test("MicroBlock cannot be created with zero transactions") {
    val transactions       = Seq.empty[TransferTransaction]
    val eitherBlockOrError = MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, None, None)

    eitherBlockOrError should produce("cannot create empty MicroBlock")
  }

  test("MicroBlock cannot contain more than Miner.MaxTransactionsPerMicroblock") {
    val transaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, Waves, 5, Waves, 1000, ByteStr.empty, System.currentTimeMillis()).explicitGet()
    val transactions = Seq.fill(Miner.MaxTransactionsPerMicroblock + 1)(transaction)

    val eitherBlockOrError = MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, None, None)
    eitherBlockOrError should produce("too many txs in MicroBlock")
  }
}
