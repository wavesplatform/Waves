package com.wavesplatform.lang.v1

import com.wavesplatform.common.utils._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.account.KeyPair
import com.wavesplatform.state._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.it.util._
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._

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
    val assetScript = ScriptCompiler.compile(
            """
               |{-# STDLIB_VERSION 2 #-}
               |{-# CONTENT_TYPE EXPRESSION #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |
               |false
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

//    val transFailed = miner.transfer(issuer, miner.address, 1, assetId = Some(assetId), fee = 1.waves, waitForTx = true)

    miner.putData(issuer, List[DataEntry[_]](
      IntegerDataEntry("int", 100500L),
      StringDataEntry("str", "text"),
      BinaryDataEntry("bin", ByteStr(Base58.decode("r1Mw3j9J"))),
      BooleanDataEntry("bool", true)), 1.waves, waitForTx = true)

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
          |	attachment = Unit
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
  }
}
