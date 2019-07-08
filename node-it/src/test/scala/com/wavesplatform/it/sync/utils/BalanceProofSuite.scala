package com.wavesplatform.it.sync.utils

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto.Merkle
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, WaitForHeight2}
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}
import play.api.libs.json.Json

class BalanceProofSuite extends FunSuite with Matchers with NodesFromDocker with WaitForHeight2 with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .build()

  val faucetNode = nodes(0)
  val faucet     = nodes(0).privateKey

  //Addresses and balances taken from genesis config
  val balances = List(
    (Address.fromString("3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC").explicitGet(), 10000000000000L),
    (Address.fromString("3HRVTkn9BdxxQJ6PDr2qibXVdTtK2D5uzRF").explicitGet(), 60000000000000L),
    (Address.fromString("3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s").explicitGet(), 25000000000000L),
    (Address.fromString("3HhtyiszMEhXdWzGgvxcfgfJdzrgfgyWcQq").explicitGet(), 45000000000000L),
    (Address.fromString("3Hi5pLwXXo3WeGEg2HgeDcy4MjQRTgz7WRx").explicitGet(), 40000000000000L),
    (Address.fromString("3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB").explicitGet(), 25000000000000L),
    (Address.fromString("3HQvEJwjxskvcKLC79XpQk6PQeNxGibozrq").explicitGet(), 80000000000000L),
    (Address.fromString("3HnGfdhUuA948dYjQHnrz2ZHxfT4P72oBBy").explicitGet(), 100000000000000L),
    (Address.fromString("3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG").explicitGet(), 15000000000000L),
    (Address.fromString("3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k").explicitGet(), 6000000000000000L)
  )

  test("Should return correct proof") {
    val height = faucetNode.height

    val tree = Merkle.mkBalanceTree(balances)

    faucetNode.waitForHeight(height + 2)

    balances
      .foreach {
        case tup @ (addr, _) =>
          val response = faucetNode
            .get(s"/addresses/balance/proof/${addr.stringRepr}/$height")
            .getResponseBody

          val actual   = (Json.parse(response) \ "proofForRegularBalance").as[String]
          val expected = Base64.encode(Merkle.proofBytes(tree.getProofForBalance(tup).get))

          actual shouldBe expected
      }
  }
}
