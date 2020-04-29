package com.wavesplatform.it.sync.grpc

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, NodeConfigs, ReportingTestName}
import org.scalatest.{FreeSpec, Matchers}

class AccountsApiGrpcSuite
    extends FreeSpec
    with Matchers
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with GrpcIntegrationSuiteWithThreeAddress {

  "getBalances returns all balances" in {
    val asset = sender.broadcastIssue(firstAcc, "non_nft_asset", 100, 8, reissuable = true, issueFee, waitForTx = true)
    val nft   = sender.broadcastIssue(firstAcc, "nft_asset", 1, 0, reissuable = false, issueFee, waitForTx = true)

    val balances = sender.assetsBalance(firstAddress)
    val nftList  = sender.nftList(firstAddress)

    balances should contain(asset.id -> 100)
    balances should have size 2 // waves and asset

    nftList should contain(nft.id)
    nftList should have size 1 // only nft
  }

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()
}
