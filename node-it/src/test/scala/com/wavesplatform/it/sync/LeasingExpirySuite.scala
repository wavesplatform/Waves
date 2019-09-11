package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatures

import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class LeasingExpirySuite extends FreeSpec with Matchers with CancelAfterFailure with NodesFromDocker with ActivationStatusRequest with ScorexLogging {
  import LeasingExpirySuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def notMiner: Node = nodes.last

  "generating balance reflects leasing expiration" in {
    notMiner.transfer(notMiner.address, address, 900.waves, minFee, waitForTx = true).id

    val lease              = notMiner.lease(notMiner.address, address, 10000.waves, minFee, version = 2, waitForTx = true).id
    val leaseHeight        = notMiner.transactionInfo(lease).height
    val cancellationHeight = leaseHeight + leaseTerm

    log.info(s"Lease height: $leaseHeight, cancellation height: $cancellationHeight")

    notMiner.waitForHeight(cancellationHeight - 1, 8.minutes)
    notMiner.balanceDetails(address).generating should be(10900.waves)

    notMiner.waitForHeight(cancellationHeight)
    notMiner.balanceDetails(address).generating should be(900.waves)

    nodes.waitForHeightArise()

    assertBadRequest(notMiner.cancelLease(notMiner.address, lease, minFee))

    val secondLease            = notMiner.lease(notMiner.address, address, 20000.waves, minFee, version = 2, waitForTx = true).id
    val secondLeaseHeight      = notMiner.transactionInfo(secondLease).height
    val secondActivationHeight = secondLeaseHeight + confirmationDepth

    log.info(s"Lease height: $secondLeaseHeight, activation height: $secondActivationHeight")

    notMiner.rollback(cancellationHeight - 5)
    notMiner.balanceDetails(address).generating should be(10900.waves)

    notMiner.waitForHeight(secondActivationHeight - 5, 8.minutes)
    notMiner.balanceDetails(address).generating should be(900.waves)

    notMiner.waitForHeight(secondActivationHeight, 1.minute)
    notMiner.balanceDetails(address).generating should be(20900.waves)
  }
}

object LeasingExpirySuite {
  import com.wavesplatform.it.NodeConfigs._

  private val confirmationDepth = 50
  private val activationHeight  = 0
  private val leaseTerm         = confirmationDepth + 5

  private val seed        = "6YvUFcg"
  private val accountSeed = "777LvAHMapxHFk2xXTSgVqZeUZ9R92LUNaSTVZGPkJ3T"
  private val privateKey  = "3j8JBY4vHKs5Y6DrQCHByVDgzUochQifFGz1u6mXKQ88"
  private val publicKey   = "GvigYJXmmVrkKNY9hHMBt9Kfob12JNqFgvK9z3weiSEW"
  private val address     = "3HUDC5h6b8RWz7hmCMvWuLkxrkkvRJoetCV"

  private val commonConfigPart: String =
    s"""
       |waves {
       |  blockchain.custom {
       |    functionality = {
       |      pre-activated-features = {
       |        ${BlockchainFeatures.SmallerMinimalGeneratingBalance.id} = $activationHeight
       |        ${BlockchainFeatures.LeaseExpired.id} = $activationHeight
       |      }
       |      generation-balance-depth-from-50-to-1000-after-height = 10000
       |      lease-expired = $leaseTerm
       |    }
       |    genesis = {
       |      average-block-delay = 5s
       |    }
       |    miner = {
       |      quorum = 0
       |    }
       |  }
       |}
       |""".stripMargin

  private val config: Config = ConfigFactory.parseString(commonConfigPart)

  private val customConfig: Config =
    ConfigFactory.parseString(
      s"""
         |$commonConfigPart
         |waves.wallet.seed = $seed
         |account-seed = $accountSeed
         |private-key = $privateKey
         |public-key = $publicKey
         |address = $address
         |""".stripMargin
    )

  val Configs: Seq[Config] = Seq(
    customConfig.withFallback(Default.head),
    config.withFallback(Default(1)),
    config.withFallback(NotMiner)
  )
}
