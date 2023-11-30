package com.wavesplatform.lagonaki.unit

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.serialization.MicroBlockSerializer
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.mining.Miner
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.*
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

  test("MicroBlock with txs bytes/parse roundtrip") {

    val ts = System.currentTimeMillis() - 5000
    val tr: TransferTransaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, Waves, 5, Waves, 2, ByteStr.empty, ts + 1).explicitGet()
    val assetId = IssuedAsset(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, assetId, 5, Waves, 2, ByteStr.empty, ts + 2).explicitGet()

    val transactions = Seq(tr, tr2)

    val microBlock  = MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, Some(stateHash)).explicitGet()
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

  test("MicroBlock cannot be created with zero transactions") {

    val transactions       = Seq.empty[TransferTransaction]
    val eitherBlockOrError = MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, None)

    eitherBlockOrError should produce("cannot create empty MicroBlock")
  }

  test("MicroBlock cannot contain more than Miner.MaxTransactionsPerMicroblock") {

    val transaction =
      TransferTransaction.selfSigned(1.toByte, sender, gen.toAddress, Waves, 5, Waves, 1000, ByteStr.empty, System.currentTimeMillis()).explicitGet()
    val transactions = Seq.fill(Miner.MaxTransactionsPerMicroblock + 1)(transaction)

    val eitherBlockOrError = MicroBlock.buildAndSign(3.toByte, sender, transactions, prevResBlockSig, totalResBlockSig, None)
    eitherBlockOrError should produce("too many txs in MicroBlock")
  }
}
