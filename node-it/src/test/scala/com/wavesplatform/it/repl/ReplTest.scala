package com.wavesplatform.it.repl

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ReplTest extends BaseSuite {
  override def nodeConfigs =
    com.wavesplatform.it.NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures(BlockchainFeatures.BlockV5.id.toInt -> 0))
      .withDefault(1)
      .buildNonConflicting()

  def await[A](f: Future[A]): A = Await.result(f, 2 seconds)

  "waves context" in {
    val issuer = miner.createAddress()
    val sample = miner.createAddress()
    val ikey = KeyPair(Base58.decode(miner.seed(issuer)))
    val trans = miner.transfer(miner.address, issuer, 100.waves, 1.waves, version = TxVersion.V3, waitForTx = true)
    miner.transfer(miner.address, sample, 100.waves, 1.waves, waitForTx = true)
    miner.createAlias(miner.address, "aaaa", waitForTx = true)

    val failDApp = ScriptCompiler.compile(
            """
               |{-# STDLIB_VERSION 4 #-}
               |{-# CONTENT_TYPE DAPP #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |
               |@Callable(i)
               |func default() = throw("")
               |""".stripMargin,
            ScriptEstimatorV3
          )
          .explicitGet()
          ._1
          .bytes()
          .base64

    val assetScript = ScriptCompiler.compile(
            """
               |{-# STDLIB_VERSION 2 #-}
               |{-# CONTENT_TYPE EXPRESSION #-}
               |{-# SCRIPT_TYPE ASSET #-}
               |
               | false
               |""".stripMargin,
            ScriptEstimatorV3
          )
          .explicitGet()
          ._1
          .bytes()
          .base64


    val assetId =
      miner.broadcastIssue(ikey, "asset", "description", 1000, decimals = 1, reissuable = true, script = Some(assetScript), waitForTx = true, fee = 1.waves).id
    val height = miner.height

    miner.putData(issuer, List[DataEntry[_]](
      IntegerDataEntry("int", 100500L),
      StringDataEntry("str", "text"),
      BinaryDataEntry("bin", ByteStr(Base58.decode("r1Mw3j9J"))),
      BooleanDataEntry("bool", true)), 1.waves, waitForTx = true)

    miner.setScript(issuer, Some(failDApp), 1.waves, waitForTx = true)

    val transFailed = miner.invokeScript(
        issuer,
        issuer,
        func = None,
        payment = Seq(),
        fee = 1.waves,
        waitForTx = true
      )
 
    val idFailed = transFailed._1.id

    (miner.rawTransactionInfo(idFailed) \ "applicationStatus").as[String] shouldBe "scriptExecutionFailed"

    val settings = NodeConnectionSettings(miner.nodeApiEndpoint.toString, 'I'.toByte, issuer)
    val repl = Repl(Some(settings))

    await(repl.execute(""" this.getInteger("int") """))  shouldBe Right("res1: Int|Unit = 100500")
    await(repl.execute(""" this.getString("str") """))   shouldBe Right("""res2: String|Unit = "text"""")
    await(repl.execute(""" this.getBinary("bin") """))   shouldBe Right("res3: ByteVector|Unit = base58'r1Mw3j9J'")
    await(repl.execute(""" this.getBoolean("bool") """)) shouldBe Right("res4: Boolean|Unit = true")

    await(repl.execute(""" height """)).explicitGet() should fullyMatch regex "res5: Int = \\d+".r

    await(repl.execute(s""" transferTransactionById(base58'${trans.id}') """)).explicitGet() should fullyMatch regex
        s"""
          |res6: TransferTransaction\\|Unit = TransferTransaction\\(
          |	recipient = Address\\(
          |		bytes = base58'$issuer'
          |	\\)
          |	timestamp = ${trans.timestamp}
          |	bodyBytes = base58'[$Base58Alphabet]+'
          |	assetId = Unit
          |	feeAssetId = Unit
          |	amount = ${trans.amount.get}
          |	version = ${trans.version.get}
          |	id = base58'${trans.id}'
          |	senderPublicKey = base58'[$Base58Alphabet]+'
          |	attachment = base58''
          |	sender = Address\\(
          |		bytes = base58'${trans.sender.get}'
          |	\\)
          |	proofs = \\[base58'[$Base58Alphabet]+', base58'', base58'', base58'', base58'', base58'', base58'', base58''\\]
          |	fee = ${trans.fee}
          |\\)
        """.trim.stripMargin

    await(repl.execute(s""" transactionHeightById(base58'$assetId') """)) shouldBe
      Right(s"res7: Int|Unit = $height")

    await(repl.execute(s""" assetInfo(base58'$assetId') """)) shouldBe
      Right(
        s"""
          |res8: Asset|Unit = Asset(
          |	name = "asset"
          |	quantity = 1000
          |	description = "description"
          |	issuer = Address(
          |		bytes = base58'$issuer'
          |	)
          |	scripted = true
          |	issuerPublicKey = base58'${Base58.encode(ikey.publicKey.arr)}'
          |	minSponsoredFee = Unit
          |	id = base58'$assetId'
          |	decimals = 1
          |	reissuable = true
          |)
        """.trim.stripMargin
      )

    await(repl.execute(s""" blockInfoByHeight($height) """)).explicitGet() should fullyMatch regex
        s"""
          |res9: BlockInfo\\|Unit = BlockInfo\\(
          |	baseTarget = \\d+
          |	generator = Address\\(
          |		bytes = base58'[$Base58Alphabet]+${"" /*miner.address*/}'
          |	\\)
          |	timestamp = \\d+
          |	vrf = base58'[$Base58Alphabet]+'
          |	height = $height
          |	generationSignature = base58'[$Base58Alphabet]+'
          |	generatorPublicKey = base58'[$Base58Alphabet]+${"" /*miner.publicKey*/}'
          |\\)
        """.trim.stripMargin

    await(repl.execute(
     s""" addressFromRecipient(Alias("aaaa")) ==
          addressFromRecipient(Address(base58'${miner.address}'))
      """
    )) shouldBe
      Right("res10: Boolean = true")

    await(repl.execute(
     s""" assetBalance(
            Address(base58'${issuer}'),
            base58'${assetId}'
          )
       """
    )).explicitGet() shouldBe "res11: Int = 1000"

    await(repl.execute(s""" wavesBalance(Address(base58'${sample}')).regular """)) shouldBe Right(s"res12: Int = ${100.waves}")
    await(repl.execute(""" this.wavesBalance() """)).explicitGet() should fullyMatch regex "res13: BalanceDetails = BalanceDetails\\(\\s+available = \\d+\\s+regular = \\d+\\s+generating = \\d+\\s+effective = \\d+\\s+\\)".r


    await(repl.execute(s""" transactionHeightById(base58'$idFailed') """)) shouldBe
      Right("res14: Int|Unit = Unit")

/* It function removed from node API. Wait native protobufs implementation. */
//    await(repl.execute(s""" transferTransactionFromProto(base58'3nec5yth17jNrNgA7dfbbmzJTKysfVyrbkAH5A8w8ncBtWYGgfxEn5hGMnNKQyacgGxuoT9DQdbufGBybzPEpR4SFSbM2o1rxgLUtocDdzLWdbSAUKKHM7f2fsCDqEExkGF2f7Se6Tfi44y3yuNMTYAKrfShEBrKGzCgbEaJtLoZo4bPdnX5V6K2eWCBFnmFjUjA947TckxnNGboh7CL6') """)) shouldBe Right(
//      s"""|res15: TransferTransaction|Unit = TransferTransaction(
//          |	recipient = Address(
//          |		bytes = base58'3HdNRU6DwZBy3ZYAmNEkncQFJFCN5DCY1FQ'
//          |	)
//          |	timestamp = 15872737
//          |	bodyBytes = base58'VgZFeoUbnDNf9w4VBwyTUPxNvhPXJwZGnqinAeszLjngHW3MGWU1y2PemPTfVvtvzvGmGieCjNqpCkVspycSPdbpVLX9CkxzdZ6HR1MxoMNWamXHESqhmy'
//          |	assetId = Unit
//          |	feeAssetId = Unit
//          |	amount = 27603095
//          |	version = 1
//          |	id = base58'EmfqfvR3CcaSitJ5AoZdrKs6AAEcWeivNi3aUT9YZaXG'
//          |	senderPublicKey = base58'CgQJiVQ73HQRgVZErv1Uri5n6ZGKSbvrXaRgsMhj8LN6'
//          |	attachment = base58''
//          |	sender = Address(
//          |		bytes = base58'3HiHQ7gWXJZuLCtBStKjjgB8J8ZkixPuGuN'
//          |	)
//          |	proofs = [base58'5op9X8DV9c5tBmDnwZo7baGqTo2dqdH5oxvS5WL4EBJKPJKLsCA2c3mvMHjmSFwf3Yf1VLoCiT2TbicV5vr5kBft', base58'', base58'', base58'', base58'', base58'', base58'', base58'']
//          |	fee = 87195628
//          |)""".stripMargin)
  }
}
