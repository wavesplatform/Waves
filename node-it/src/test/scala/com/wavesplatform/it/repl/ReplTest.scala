package com.wavesplatform.it.repl

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.transactions.{FailedTransactionSuiteLike, OverflowBlock}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ReplTest extends BaseTransactionSuite with FailedTransactionSuiteLike[String] with OverflowBlock {
  override protected def waitForHeightArise(): Unit =
    nodes.waitForHeightArise()

  override def nodeConfigs: Seq[Config] =
    com.wavesplatform.it.NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures(BlockchainFeatures.BlockV5.id.toInt -> 0))
      .withDefault(1)
      .buildNonConflicting()

  def await[A](f: Future[A]): A = Await.result(f, 2 seconds)

  test("waves context") {
    val issuer = miner.createKeyPair()
    val sample = miner.createKeyPair()
    val trans  = miner.transfer(miner.keyPair, issuer.toAddress.toString, 100.waves, 1.waves, version = TxVersion.V3, waitForTx = true)
    miner.transfer(miner.keyPair, sample.toAddress.toString, 100.waves, 1.waves, waitForTx = true)
    miner.createAlias(miner.keyPair, "aaaa", waitForTx = true)

    val failDApp = ScriptCompiler
      .compile(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func default() = {
           |  let action = valueOrElse(getString(this, "crash"), "no")
           |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 10} true
           |
           |  if (action == "yes")
           |  then {
           |    if (check)
           |    then throw("Crashed by dApp")
           |    else throw("Crashed by dApp")
           |  }
           |  else []
           |}
           |
           |""".stripMargin,
        ScriptEstimatorV3.latest
      )
      .explicitGet()
      ._1
      .bytes()
      .base64

    val assetScript = ScriptCompiler
      .compile(
        """
          |{-# STDLIB_VERSION 2 #-}
          |{-# CONTENT_TYPE EXPRESSION #-}
          |{-# SCRIPT_TYPE ASSET #-}
          |
          | false
          |""".stripMargin,
        ScriptEstimatorV3.latest
      )
      .explicitGet()
      ._1
      .bytes()
      .base64

    val assetId =
      miner
        .broadcastIssue(
          issuer,
          "asset",
          "description",
          1000,
          decimals = 1,
          reissuable = true,
          script = Some(assetScript),
          waitForTx = true,
          fee = 1.waves
        )
        .id
    val height = miner.transactionStatus(assetId).height.get

    miner.putData(
      issuer,
      List[DataEntry[_]](
        IntegerDataEntry("int", 100500L),
        StringDataEntry("str", "text"),
        BinaryDataEntry("bin", ByteStr(Base58.decode("r1Mw3j9J"))),
        BooleanDataEntry("bool", true)
      ),
      1.waves,
      waitForTx = true
    )

    miner.setScript(issuer, Some(failDApp), 1.waves, waitForTx = true)

    val settings = NodeConnectionSettings(miner.nodeApiEndpoint.toString, 'I'.toByte, issuer.toAddress.toString)
    val repl     = Repl(Some(settings))

    await(repl.execute(""" this.getInteger("int") """)) shouldBe Right("res1: Int|Unit = 100500")
    await(repl.execute(""" this.getString("str") """)) shouldBe Right("""res2: String|Unit = "text"""")
    await(repl.execute(""" this.getBinary("bin") """)) shouldBe Right("res3: ByteVector|Unit = base58'r1Mw3j9J'")
    await(repl.execute(""" this.getBoolean("bool") """)) shouldBe Right("res4: Boolean|Unit = true")

    await(repl.execute(""" height """)).explicitGet() should fullyMatch regex "res5: Int = \\d+".r

    await(repl.execute(s""" transferTransactionById(base58'${trans.id}') """)).explicitGet() should fullyMatch regex
      s"""
          |res6: TransferTransaction\\|Unit = TransferTransaction\\(
          |	recipient = Address\\(
          |		bytes = base58'${issuer.toAddress}'
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
          |	description = "description"
          |	issuer = Address(
          |		bytes = base58'${issuer.toAddress}'
          |	)
          |	scripted = true
          |	issuerPublicKey = base58'${issuer.publicKey}'
          |	minSponsoredFee = Unit
          |	id = base58'$assetId'
          |	decimals = 1
          |	reissuable = true
          |	name = "asset"
          |	quantity = 1000
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
          |	generationSignature = base58'[$Base58Alphabet]+'
          |	generatorPublicKey = base58'[$Base58Alphabet]+${"" /*miner.publicKey*/}'
          |	height = $height
          |	rewards = \\[\\]
          |\\)
        """.trim.stripMargin

    await(
      repl.execute(
        s""" addressFromRecipient(Alias("aaaa")) ==
          addressFromRecipient(Address(base58'${miner.address}'))
      """
      )
    ) shouldBe
      Right("res10: Boolean = true")

    await(
      repl.execute(
        s""" assetBalance(
            Address(base58'${issuer.toAddress}'),
            base58'$assetId'
          )
       """
      )
    ).explicitGet() shouldBe "res11: Int = 1000"

    await(repl.execute(s""" wavesBalance(Address(base58'${sample.toAddress}')).regular """)) shouldBe Right(s"res12: Int = ${100.waves}")
    await(repl.execute(""" this.wavesBalance() """))
      .explicitGet() should fullyMatch regex "res13: BalanceDetails = BalanceDetails\\(\\s+available = \\d+\\s+regular = \\d+\\s+generating = \\d+\\s+effective = \\d+\\s+\\)".r

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
