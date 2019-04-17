package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig.{matcherConfig, minerDisabled, minerEnabled}
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderV1}

import scala.concurrent.duration.DurationInt
import scala.util.Random

class BroadcastUntilConfirmedTestSuite extends MatcherSuiteBase {
  private def broadcastConfig =
    ConfigFactory
      .parseString(s"""waves.matcher.exchange-transaction-broadcast {
                                                           |  broadcast-until-confirmed = yes
                                                           |  interval = 20s
                                                           |}""".stripMargin)
      .withFallback(matcherConfig)

  override protected def nodeConfigs: Seq[Config] =
    (Default.last +: Random.shuffle(Default.init).take(2))
      .zip(Seq(broadcastConfig, minerDisabled, minerEnabled))
      .map { case (n, o) => o.withFallback(n) }

  private def matcherPublicKey = matcherNode.publicKey
  private def minerDockerNodes = dockerNodes().filterNot(_ == matcherNode)

  private val asset = IssueTransactionV1
    .selfSigned(
      sender = aliceAcc,
      name = Random.nextString(4).getBytes(),
      description = Random.nextString(10).getBytes(),
      quantity = Long.MaxValue,
      decimals = 8,
      reissuable = false,
      fee = issueFee,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  "BroadcastUntilConfirmed" in {
    markup("Issue an asset")
    matcherNode.signedBroadcast(asset.json())
    val pair = AssetPair(Some(asset.id()), None)
    nodes.waitForTransaction(asset.id().base58)
    nodes.waitForHeightArise()

    markup("Prepare orders")
    val now = System.currentTimeMillis()
    val alicePlace = OrderV1.sell(
      sender = aliceAcc, // lazy val ...
      matcher = matcherPublicKey,
      pair = pair,
      amount = 100000L,
      price = 80000L,
      timestamp = now,
      expiration = now + 1.day.toMillis,
      matcherFee = 300000L
    )

    val bobPlace = OrderV1.buy(
      sender = bobAcc,
      matcher = matcherPublicKey,
      pair = pair,
      amount = 200000L,
      price = 100000L,
      timestamp = now,
      expiration = now + 1.day.toMillis,
      matcherFee = 300000L
    )

    markup("Shutdown miners")
    val minerContainerIds = minerDockerNodes.map(docker.stopContainer)

    markup("Place orders, those should match")
    matcherNode.placeOrder(alicePlace)
    matcherNode.placeOrder(bobPlace)
    matcherNode.waitOrderStatus(pair, alicePlace.idStr(), "Filled")
    val exchangeTxId = matcherNode.transactionsByOrder(alicePlace.idStr()).head.id

    markup("Start miners and wait until it receives the transaction")
    minerContainerIds.foreach(docker.startContainer)
    nodes.waitForTransaction(exchangeTxId)
  }
}
