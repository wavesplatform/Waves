package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.{Block, BlockHeader, SignerData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.CancelAfterFailure

class RIDEFuncSuite extends BaseTransactionSuite with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw(s"""
                           |waves.blockchain.custom.functionality.pre-activated-features = {
                           |          2 = 0
                           |          3 = 0
                           |          4 = 0
                           |          5 = 0
                           |          6 = 0
                           |          7 = 0
                           |          9 = 0
                           |          10 = 0
                           |          11 = 0
                           |          12 = 0
                           |          13 = 0
                           |          14 = 0
                           |}
         """.stripMargin))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private val acc0 = pkByAddress(firstAddress)

  test("assetBalance() verification") {
    val asset = sender
      .issue(acc0.address, "SomeCoin", "SomeDescription", someAssetAmount, 0, reissuable = false, issueFee, 2, waitForTx = true)
      .id

    val newAddress   = sender.createAddress()
    val pkNewAddress = pkByAddress(newAddress)

    sender.transfer(acc0.address, newAddress, 10.waves, minFee, waitForTx = true)

    val scriptSrc =
      s"""
         |match tx {
         |  case tx : SetScriptTransaction => true
         |  case other => assetBalance(tx.sender, base58'$asset') > 0
         |}
      """.stripMargin

    val compiled = ScriptCompiler(scriptSrc, isAssetScript = false).explicitGet()._1

    val tx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(pkNewAddress, Some(compiled), setScriptFee, System.currentTimeMillis()).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
          .explicitGet()
          .json()),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(IssuedAsset(ByteStr.decodeBase58(asset).get),
                    acc0,
                    pkNewAddress,
                    100000000,
                    System.currentTimeMillis(),
                    Waves,
                    smartMinFee,
                    Array())
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transfer = sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
        .explicitGet()
        .json())
    nodes.waitForHeightAriseAndTxPresent(transfer.id)

    val udpatedScript =
      s"""
         |match tx {
         |  case tx : SetScriptTransaction => true
         |  case other => assetBalance(tx.sender, base58'$asset') >= 900000000 && wavesBalance(tx.sender) >500000000
         |}
      """.stripMargin

    val updated = ScriptCompiler(udpatedScript, isAssetScript = false).explicitGet()._1

    val updTx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(pkNewAddress, Some(updated), setScriptFee + smartFee, System.currentTimeMillis()).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(updTx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
          .explicitGet()
          .json()),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(IssuedAsset(ByteStr.decodeBase58(asset).get),
                    acc0,
                    pkNewAddress,
                    800000000,
                    System.currentTimeMillis(),
                    Waves,
                    smartMinFee,
                    Array())
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transferAfterUpd = sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
        .explicitGet()
        .json())
    nodes.waitForHeightAriseAndTxPresent(transferAfterUpd.id)
  }

  test("split around empty string") {
    val scriptText =
      s"""
         |  {-# STDLIB_VERSION 3       #-}
         |  {-# CONTENT_TYPE   DAPP    #-}
         |  {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |  @Verifier(tx)
         |  func verify() = {
         |    let strs = split("some", "")
         |    strs.size() == 4  &&
         |    strs[0] == "s"    &&
         |    strs[1] == "o"    &&
         |    strs[2] == "m"    &&
         |    strs[3] == "e"
         |  }
      """.stripMargin

    val compiledScript = ScriptCompiler.compile(scriptText).explicitGet()._1

    val newAddress   = sender.createAddress()
    val pkNewAddress = pkByAddress(newAddress)
    sender.transfer(acc0.address, newAddress, 10.waves, minFee, waitForTx = true)

    val scriptSet = SetScriptTransaction.selfSigned(
      pkNewAddress,
      Some(compiledScript),
      setScriptFee,
      System.currentTimeMillis()
    )
    val scriptSetBroadcast = sender.signedBroadcast(scriptSet.explicitGet().json.value)
    nodes.waitForHeightAriseAndTxPresent(scriptSetBroadcast.id)

    val transfer = TransferTransactionV2.selfSigned(
      Waves,
      pkNewAddress,
      pkNewAddress,
      1.waves,
      System.currentTimeMillis(),
      Waves,
      smartMinFee,
      Array()
    )
    val transferBroadcast = sender.signedBroadcast(transfer.explicitGet().json.value)
    nodes.waitForHeightAriseAndTxPresent(transferBroadcast.id)
  }

  test("lastBlock and blockInfoByHeight(last) must return liquid block") {
    val script = ScriptCompiler.compile(s"""
         |  {-# STDLIB_VERSION 3       #-}
         |  {-# CONTENT_TYPE   DAPP    #-}
         |  {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |  @Verifier(tx)
         |  func verify() = {
         |    let block = extract(blockInfoByHeight(height))
         |
         |    let checkTs = lastBlock.timestamp == block.timestamp
         |    let checkHeight = block.height == height
         |    let checkHeightLast = lastBlock.height == height
         |    checkTs && checkHeight && checkHeightLast
         |  }
      """.stripMargin).explicitGet()._1

    val newAddress = sender.createAddress()
    sender.transfer(acc0.address, newAddress, 10.waves, minFee, waitForTx = true)

    val setScript = sender.setScript(newAddress, Some(script.bytes().base64), setScriptFee)
    nodes.waitForHeightAriseAndTxPresent(setScript.id)

    val transfer = sender.transfer(newAddress, newAddress, 1.waves, minFee + (2 * smartFee))
    nodes.waitForHeightAriseAndTxPresent(transfer.id)
  }

  test("parseBlockHeader() must work!") {
    val blockheaderGen: Gen[BlockHeader] = {
      for {
        timestamp        <- Gen.posNum[Long]
        version          <- Gen.posNum[Byte]
        reference        <- Gen.containerOfN[Array, Byte](SignatureLength, Arbitrary.arbByte.arbitrary)
        generator        <- Gen.containerOfN[Array, Byte](KeyLength, Arbitrary.arbByte.arbitrary)
        signature        <- Gen.containerOfN[Array, Byte](SignatureLength, Arbitrary.arbByte.arbitrary)
        baseTarget       <- Gen.posNum[Long]
        genSignature     <- Gen.containerOfN[Array, Byte](Block.GeneratorSignatureLength, Arbitrary.arbByte.arbitrary)
        transactionCount <- Gen.posNum[Int]
        featureVotes     <- Gen.listOf(Gen.posNum[Short])
      } yield {
        new BlockHeader(
          timestamp,
          version,
          reference,
          SignerData(PublicKey(generator), signature),
          NxtLikeConsensusBlockData(baseTarget, genSignature),
          transactionCount,
          featureVotes.toSet
        )
      }
    }

    def script(header: BlockHeader): Script = {
      val expectedReference    = Base64.encode(header.reference)
      val expectedGenerator    = Base64.encode(header.signerData.generator.bytes)
      val expectedGeneratorPK  = Base64.encode(header.signerData.generator.toAddress.bytes)
      val expectedSignature    = Base64.encode(header.signerData.signature)
      val expectedGenSignature = Base64.encode(header.consensusData.generationSignature)

      ScriptCompiler.compile(
        s"""
           |  {-# STDLIB_VERSION 4       #-}
           |  {-# CONTENT_TYPE   DAPP    #-}
           |  {-# SCRIPT_TYPE    ACCOUNT #-}
           |
           |  @Callable(i)
           |  func test(headerBytes: ByteVector)  = {
           |    match headerBytes.parseBlockHeader() {
           |      case header: BlockHeader =>
           |        let headerValid =
           |          header.timestamp == ${header.timestamp} &&
           |            header.version == ${header.version} &&
           |            header.reference == base64'$expectedReference' &&
           |            header.generator == base64'$expectedGenerator' &&
           |            header.generatorPublicKey == base64'$expectedGeneratorPK' &&
           |            header.signature == base64'$expectedSignature' &&
           |            header.baseTarget == ${header.consensusData.baseTarget} &&
           |            header.generationSignature == base64'$expectedGenSignature' &&
           |            header.transactionCount == ${header.transactionCount}
           |        if headerValid
           |        then WriteSet([
           |          DataEntry("k", "v")
           |        ])
           |        else throw("Parsed invalid header!")
           |      case _ => throw("Can't parse header!")
           |    }
           |  }
           |""".stripMargin
      )
    }.explicitGet()._1

    val newAddress = sender.createAddress()
    sender.transfer(acc0.address, newAddress, 10.waves, minFee, waitForTx = true)

    val header: BlockHeader = blockheaderGen.sample.get

    val setScript = sender.setScript(newAddress, Some(script(header).bytes().base64), setScriptFee)
    nodes.waitForHeightAriseAndTxPresent(setScript.id)

    val invokeTx = sender.invokeScript(
      sender.address,
      newAddress,
      Some("test"),
      List(
        CONST_BYTESTR(BlockHeader.writeHeaderOnly(header)).explicitGet()
      )
    )

    nodes.waitForHeightAriseAndTxPresent(invokeTx.id)
  }
}
