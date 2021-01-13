package com.wavesplatform.it.async

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, TransferSending}
import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.mining.MiningConstraints.MaxScriptRunsInBlock
import com.wavesplatform.transaction.smart.SetScriptTransaction
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SmartTransactionsConstraintsSuite extends FreeSpec with Matchers with TransferSending with NodesFromDocker {

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(
      _.raw(
        s"""akka.http.server {
         |  parsing.max-content-length = 3737439
         |  request-timeout = 60s
         |}
         |
         |waves {
         |  miner.quorum = 0
         |
         |  blockchain.custom {
         |    functionality {
         |      pre-activated-features {
         |        2 = 0
         |        4 = 0
         |        11 = 100500
         |      }
         |    }
         |  }
         |}""".stripMargin
      )
    )
    .withDefault(1)
    .build(false)

  private def miner           = nodes.head
  private val smartPrivateKey = KeyPair.fromSeed(NodeConfigs.Default(1).getString("account-seed")).explicitGet()

  s"Block is limited by size after activation" in result(
    for {
      _          <- miner.signedBroadcast(setScriptTx(smartPrivateKey).json())
      firstHeight <- miner.waitForHeightArise
      _           <- processRequests(generateTransfersFromAccount(MaxScriptRunsInBlock * 3, smartPrivateKey.toAddress.toString))
      _           <- miner.waitForEmptyUtx()
      lastHeight  <- miner.waitForHeightArise
      headers     <- miner.blockHeadersSeq(firstHeight, lastHeight)
    } yield headers.foreach { x =>
      x.transactionCount should (be <= MaxScriptRunsInBlock)
    },
    12.minutes
  )

  private def setScriptTx(sender: KeyPair) =
    SetScriptTransaction
      .selfSigned(
        1.toByte,
        sender = sender,
        script = Some(ExprScript(V1, Terms.TRUE, checkSize = false).explicitGet()),
        fee = 1000000,
        timestamp = System.currentTimeMillis() - 5.minutes.toMillis
      )
      .explicitGet()
}
