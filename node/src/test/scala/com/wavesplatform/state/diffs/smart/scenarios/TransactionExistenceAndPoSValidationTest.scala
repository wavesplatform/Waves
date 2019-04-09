package com.wavesplatform.state.diffs.smart.scenarios

import com.google.common.primitives.Ints
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, V4}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.BinaryDataEntry
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.{CreateAliasTransactionV1, DataTransaction, GenesisTransaction}
import com.wavesplatform.utils.Merkle
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.{Digest, Digest32}

import scala.util.Random

class TransactionExistenceAndPoSValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val AMT: Long = 1000000 * 100000000L

  val fs =
    TestFunctionalitySettings.Stub
      .copy(
        preActivatedFeatures = BlockchainFeatures.implemented.map(_ -> 0).toMap
      )

  def gen(miner: KeyPair, gateway: KeyPair, script: Script): Gen[Seq[Block]] = {
    val ts        = System.currentTimeMillis()
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
      transferBlock    = TestBlock.create(time = ts + 1, txs = Seq(transferTx), signer = miner, ref = genesisBlock.signerData.signature)
      setScriptTxBlock = TestBlock.create(time = ts + 2, txs = Seq(setScriptTx), signer = miner, ref = transferBlock.signerData.signature)
    } yield Seq(genesisBlock, transferBlock, setScriptTxBlock)
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

        forAll(gen(miner, gateway, ExprScript(V4, typedScript).explicitGet())) { blocks =>
          val dataTx =
            DataTransaction
              .selfSigned(
                gateway,
                List(
                  BinaryDataEntry("header_bytes", ByteStr(blockHeader.headerBytes()))
                ),
                5 * 10000000,
                System.currentTimeMillis()
              )
              .explicitGet()

          val blockWithData =
            TestBlock
              .create(
                signer = miner,
                txs = List(dataTx)
              )

          assertDiffEi(blocks, blockWithData, fs) { diffEi =>
            diffEi shouldBe an[Right[_, _]]
          }
        }
    }
  }

  property("can parse transaction") {
    forAll(accountGen, accountGen, randomTransactionGen) { (miner, gateway, tx) =>
      val scriptSource =
        s"""
           |match tx {
           |  case dt: DataTransaction =>
           |    let txBytes = extract(getBinary(dt.data, "tx_bytes"))
           |    let parsedTx = extract(transactionFromBytes(txBytes))
           |
           |    parsedTx.id == base58'${tx.id().base58}'
           |
           |  case _ => false
           |}
         """.stripMargin

      val untypedScript = Parser.parseExpr(scriptSource).get.value
      val typedScript =
        ExpressionCompiler(com.wavesplatform.utils.compilerContext(V4, Expression, isAssetScript = false), untypedScript).explicitGet()._1

      forAll(gen(miner, gateway, ExprScript(V4, typedScript).explicitGet())) { blocks =>
        val dataTx =
          DataTransaction
            .selfSigned(
              gateway,
              List(
                BinaryDataEntry("tx_bytes", ByteStr(tx.bytes()))
              ),
              5 * 10000000,
              System.currentTimeMillis()
            )
            .explicitGet()

        val blockWithData =
          TestBlock
            .create(
              signer = miner,
              txs = List(dataTx)
            )

        assertDiffEi(blocks, blockWithData, fs) { diffEi =>
          diffEi shouldBe an[Right[_, _]]
        }
      }
    }
  }

  property("can check merkle proof") {
    val scriptSource =
      s"""
         |match tx {
         |  case dt: DataTransaction =>
         |    let rootHash = extract(getBinary(dt.data, "root_hash"))
         |    let leafData = extract(getBinary(dt.data, "value_bytes"))
         |    let merkleProof = extract(getBinary(dt.data, "proof"))
         |
         |    checkMerkleProof(rootHash, merkleProof, leafData)
         |
         |  case _ => false
         |}
         """.stripMargin

    val (tree, leafs) = {
      val data: List[LeafData] =
        List
          .fill(100)(Random.nextInt(10000))
          .map(Ints.toByteArray)
          .map(LeafData @@ _)

      val tree = MerkleTree[Digest32](data)(Merkle.FastHash)

      (tree, data)
    }

    forAll(accountGen, accountGen, Gen.oneOf(leafs)) { (miner, gateway, leaf) =>
      val untypedScript = Parser.parseExpr(scriptSource).get.value
      val typedScript =
        ExpressionCompiler(com.wavesplatform.utils.compilerContext(V4, Expression, isAssetScript = false), untypedScript).explicitGet()._1

      forAll(gen(miner, gateway, ExprScript(V4, typedScript).explicitGet())) { blocks =>
        val proof =
          tree
            .proofByElement(Leaf[Digest32](leaf)(Merkle.FastHash))
            .get

        val dataTx =
          DataTransaction
            .selfSigned(
              gateway,
              List(
                BinaryDataEntry("root_hash", ByteStr(tree.rootHash)),
                BinaryDataEntry("value_bytes", ByteStr(leaf)),
                BinaryDataEntry("proof", ByteStr(proofBytes(proof)))
              ),
              5 * 10000000,
              System.currentTimeMillis()
            )
            .explicitGet()

        val blockWithData =
          TestBlock
            .create(
              signer = miner,
              txs = List(dataTx)
            )

        assertDiffEi(blocks, blockWithData, fs) { diffEi =>
          assert(proof.valid(tree.rootHash))
          diffEi shouldBe an[Right[_, _]]
        }
      }
    }
  }

  property("can compute account script") {
    forAll(accountGen, accountGen, aliasGen) { (miner, gateway, alias) =>
      val minerScript = compile("""
          |true
        """.stripMargin)

      val minerScriptHash = Base58.encode(com.wavesplatform.crypto.fastHash(ExprScript(V4, minerScript).explicitGet().bytes()))

      val gatewayScript = compile(s"""
           | let alias = Alias("${alias.name}")
           | let address = Address(base58'${miner.toAddress.bytes.base58}')
           |
           | let byAlias = extract(accountScriptHash(alias))
           | let byAddress = extract(accountScriptHash(address))
           |
           | let expectedHash = base58'$minerScriptHash'
           |
           | byAlias == expectedHash &&
           |   byAddress == expectedHash
         """.stripMargin)


      forAll(gen(miner, gateway, ExprScript(V4, gatewayScript).explicitGet())) { blocks =>
        val minerScriptTx =
          SetScriptTransaction
            .selfSigned(
              miner,
              Some(ExprScript(V4, minerScript).explicitGet()),
              1000000000,
              System.currentTimeMillis()
            )
            .explicitGet()

        val minerAliasTx =
          CreateAliasTransactionV1.selfSigned(
            miner,
            alias,
            5 * 10000000,
            System.currentTimeMillis()
          ).explicitGet()

        val dataTx =
          DataTransaction
            .selfSigned(
              gateway,
              List(),
              5 * 10000000,
              System.currentTimeMillis()
            )
            .explicitGet()

        val blockWithAliasAndScript =
          TestBlock
            .create(
              signer = miner,
              txs = List(minerAliasTx, minerScriptTx)
            )

        val blockWithData =
          TestBlock
            .create(
              signer = miner,
              txs = List(dataTx)
            )

        assertDiffEi(blocks :+ blockWithAliasAndScript, blockWithData, fs) { diffEi =>
          diffEi shouldBe an[Right[_, _]]
        }
      }
    }
  }

  def compile(src: String): EXPR = {
    val untypedScript = Parser.parseExpr(src).get.value
    ExpressionCompiler(com.wavesplatform.utils.compilerContext(V4, Expression, isAssetScript = false), untypedScript).explicitGet()._1
  }

  def proofBytes(mp: MerkleProof[Digest32]): Array[Byte] = {
    def _proofBytes(lvls: List[(Digest, Side)], acc: Array[Byte]): Array[Byte] = {
      lvls match {
        case (d, s) :: xs => _proofBytes(xs, Array.concat(acc, s +: d.length.toByte +: d))
        case Nil          => acc
      }
    }

    _proofBytes(mp.levels.toList, Array.emptyByteArray)
  }
}
