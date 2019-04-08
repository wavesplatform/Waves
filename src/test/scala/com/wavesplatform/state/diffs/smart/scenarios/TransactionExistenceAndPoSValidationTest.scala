package com.wavesplatform.state.diffs.smart.scenarios

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, V4}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.{BinaryDataEntry, BlockchainUpdaterImpl, NG}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.{BlockchainUpdater, DataTransaction, GenesisTransaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.hash.Digest32

import scala.util.Random

class TransactionExistenceAndPoSValidationTest extends PropSpec with PropertyChecks with Matchers with WithDB with TransactionGen with NoShrink {

  val AMT: Long = 1000000 * 100000000L

  def gen(miner: KeyPair, gateway: KeyPair, script: Script)(time: Time): Gen[Seq[Block]] = {
    val ts        = time.correctedTime()
    val genesisTx = GenesisTransaction.create(miner, AMT, ts).explicitGet()

    for {
      transferTx <- transferGeneratorP(ts, miner, gateway, AMT / 10)
      setScriptTx = SetScriptTransaction
        .selfSigned(
          gateway,
          Some(script),
          1000000000,
          ts + 100
        )
        .explicitGet()
      genesisBlock     = TestBlock.create(time = ts, txs = Seq(genesisTx), signer = miner)
      transferBlock    = TestBlock.create(time = ts + 1000, txs = Seq(transferTx), signer = miner, ref = genesisBlock.uniqueId)
      setScriptTxBlock = TestBlock.create(time = ts + 2000, txs = Seq(setScriptTx), signer = miner, ref = transferBlock.uniqueId)
      emptyBlock       = TestBlock.create(time = ts + 3000, txs = Seq.empty, signer = miner, ref = setScriptTxBlock.uniqueId)
    } yield Seq(genesisBlock, transferBlock, setScriptTxBlock, emptyBlock)
  }

  property("can parse block header") {
    forAll(blockGen, accountGen, accountGen) {
      case ((baseTarget, reference, generationSignature, recipient, transactionData), miner, gateway) =>
        val randomHash = Digest32 @@ new Array[Byte](32)

        Random.nextBytes(randomHash)

        val blockHeader = Block
          .buildAndSign(
            3,
            System.currentTimeMillis(),
            reference,
            NxtLikeConsensusBlockData(baseTarget, generationSignature),
            transactionData,
            randomHash,
            randomHash,
            randomHash,
            recipient,
            Set.empty
          )
          .explicitGet()

        val scriptSource =
          s"""
             |match tx {
             |	case dt: DataTransaction =>
             |    let headerBytes = extract(getBinary(dt.data, "header_bytes"))
             |		let blockHeader = extract(blockHeaderFromBytes(headerBytes))
             |
             |    blockHeader.version == 3 &&
             |      blockHeader.timestamp == ${blockHeader.timestamp} &&
             |      blockHeader.reference == base58'${blockHeader.reference.base58}' &&
             |      blockHeader.signature == base58'${blockHeader.signerData.signature.base58}' &&
             |      blockHeader.generator == base58'${blockHeader.signerData.generator.base58}' &&
             |      blockHeader.generationSignature == base58'${blockHeader.consensusData.generationSignature.base58}' &&
             |      blockHeader.baseTarget == ${blockHeader.consensusData.baseTarget} &&
             |      blockHeader.transactionTreeHash == base58'${Base58.encode(blockHeader.transactionTreeHash)}' &&
             |      blockHeader.minerWavesBalancesTreeHash == base58'${Base58.encode(blockHeader.minerWavesBalancesTreeHash)}' &&
             |      blockHeader.minerEffectiveBalancesTreeHash == base58'${Base58.encode(blockHeader.minerEffectiveBalancesTreeHash)}' &&
             |      blockHeader.transactionCount == ${blockHeader.transactionCount}
             |
             |	case _ => false
             |}
           """.stripMargin

        val untypedScript = Parser.parseExpr(scriptSource).get.value
        val typedScript =
          ExpressionCompiler(com.wavesplatform.utils.compilerContext(V4, Expression, isAssetScript = false), untypedScript).explicitGet()._1

        withEnv(gen(miner, gateway, ExprScript(V4, typedScript).explicitGet())) {
          case Env(bcu, _) =>
            val lastBlock = bcu.lastBlock.get

            val dataTx =
              DataTransaction
                .selfSigned(
                  gateway,
                  List(
                    BinaryDataEntry("header_bytes", ByteStr(blockHeader.headerBytes()))
                  ),
                  5 * 10000000,
                  lastBlock.timestamp
                )
                .explicitGet()

            val blockWithData =
              TestBlock
                .create(
                  time = lastBlock.timestamp + 1000,
                  signer = miner,
                  txs = List(dataTx),
                  ref = lastBlock.uniqueId
                )

            bcu.processBlock(blockWithData) shouldBe an[Right[_, _]]
        }
    }
  }

  final case class Env(blockchain: BlockchainUpdater with NG, poSSelector: PoSSelector)

  def withEnv(gen: Time => Gen[(Seq[Block])])(f: Env => Unit): Unit = {
    val fs =
      TestFunctionalitySettings.Stub
        .copy(
          preActivatedFeatures = BlockchainFeatures.implemented.map(_ -> 0).toMap
        )
    val defaultWriter = new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, 10000, 2000, 120 * 60 * 1000)
    val settings0     = WavesSettings.fromConfig(loadConfig(ConfigFactory.load()))
    val settings      = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime)
    val pos           = new PoSSelector(bcu, settings.blockchainSettings, settings.synchronizationSettings)
    try {
      val blocks = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      f(Env(bcu, pos))
      bcu.shutdown()
    } finally {
      bcu.shutdown()
      db.close()
    }
  }
}
