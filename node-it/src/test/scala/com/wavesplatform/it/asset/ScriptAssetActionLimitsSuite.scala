package com.wavesplatform.it.asset

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures.{RideV6, SynchronousCalls}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuiteLike
import com.wavesplatform.it.sync.{issueFee, minFee, smartMinFee}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5, V6}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.freespec.AnyFreeSpec

class ScriptAssetActionLimitsSuite extends AnyFreeSpec with GrpcBaseTransactionSuiteLike {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 2, Seq.empty)
      .overrideBase(_.preactivatedFeatures((SynchronousCalls.id, 0), (RideV6.id, 0)))
      .buildNonConflicting()

  private val initialWavesBalance  = 1000.waves
  private val minSponsoredAssetFee = 1001
  private val asset                = Asset("Simple", "ReissuableAsset", "description", 100000000, reissuable = true, 3, 0)

  Seq(V4, V5, V6).foreach { version =>
    val actionsLimit =
      if (version == V6)
        ContractLimits.MaxAssetScriptActionsAmountV6
      else
        ContractLimits.MaxCallableActionsAmountBeforeV6(version)

    val errMsg =
      if (version == V6)
        "Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"
      else
        "Actions count limit is exceeded"

    s"SponsorFee actions count limit for V${version.id}" in {
      val dApp        = createSponsorFeeDApp(actionsLimit, version)
      val dAppAddress = dApp.toAddress.toString
      val invokeTx1 = miner.invokeScript(
        miner.keyPair,
        dAppAddress,
        Some(s"issue${actionsLimit}assets"),
        waitForTx = true,
        fee = smartMinFee + issueFee * actionsLimit
      )
      val invokeTx2 = miner.invokeScript(miner.keyPair, dAppAddress, Some(s"sponsor${actionsLimit}assets"), waitForTx = true, fee = smartMinFee)

      val assetIds    = miner.debugStateChanges(invokeTx1._1.id).stateChanges.get.issues.map(_.assetId)
      val sponsorFees = miner.debugStateChanges(invokeTx2._1.id).stateChanges.get.sponsorFees

      (assetIds zip sponsorFees)
        .foreach { case (issueAssetId, sponsorFee) =>
          issueAssetId shouldBe sponsorFee.assetId
          sponsorFee.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)

          miner.assetsDetails(issueAssetId).minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
          val dAppBalance = miner.assetsBalance(dAppAddress).balances.find(_.assetId == issueAssetId).get
          dAppBalance.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)
          dAppBalance.sponsorBalance shouldBe Some(miner.balance(dAppAddress).balance)
        }

      assertBadRequestAndMessage(
        miner.invokeScript(miner.keyPair, dAppAddress, Some(s"sponsor${actionsLimit + 1}assets"), fee = smartMinFee),
        errMsg
      )
    }

    s"Issue $actionsLimit assets should not produce an error for V${version.id}" in {
      val acc = createDApp(script(actionsLimit, version))
      val (tx, _) = miner.invokeScript(
        acc,
        acc.toAddress.toString,
        Some(s"issue${actionsLimit}Assets"),
        fee = smartMinFee + issueFee * actionsLimit,
        waitForTx = true
      )
      for (nth <- 0 until actionsLimit) {
        val assetInfo = sender.debugStateChanges(tx.id).stateChanges.get.issues(nth)
        assetInfo.quantity shouldBe asset.quantity
        assetInfo.name shouldBe asset.name
        assetInfo.description shouldBe asset.description
        assetInfo.decimals shouldBe asset.decimals
        assetInfo.isReissuable shouldBe asset.reissuable
        sender.assertAssetBalance(acc.toAddress.toString, assetInfo.assetId, asset.quantity)
      }
    }

    s"More than $actionsLimit Issue/Reissue/Burn/SponsorFee actions should produce an error for V${version.id}" in {
      val acc   = createDApp(script(actionsLimit, version))
      val issue = miner.issue(acc)

      assertApiError(
        miner.invokeScript(
          acc,
          acc.toAddress.toString,
          Some(s"process${actionsLimit + 1}actions"),
          List(CONST_BYTESTR(ByteStr.decodeBase58(issue.id).get).explicitGet()),
          fee = smartMinFee + issueFee * (actionsLimit + 1),
          waitForTx = true
        )
      ) { e =>
        e.message should include(errMsg)
      }
    }

    s"More than $actionsLimit issue actions should produce an error for V${version.id}" in {
      val acc = createDApp(script(actionsLimit, version))
      assertApiError(
        miner.invokeScript(
          acc,
          acc.toAddress.toString,
          Some(s"issue${actionsLimit + 1}Assets"),
          fee = smartMinFee + issueFee * (actionsLimit + 1),
          waitForTx = true
        )
      ) { e =>
        e.message should include(errMsg)
      }
    }
  }

  private def createDApp(script: String, address: KeyPair = miner.createKeyPair()): KeyPair = {
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.transfer(sender.keyPair, address.publicKey.toAddress.toString, initialWavesBalance, minFee, waitForTx = true)

    nodes.waitForHeightAriseAndTxPresent(
      miner
        .signedBroadcast(
          SetScriptTransaction
            .selfSigned(1.toByte, address, Some(compiledScript), 1.waves, System.currentTimeMillis())
            .explicitGet()
            .json()
        )
        .id
    )

    address
  }

  private def createSponsorFeeDApp(actionsLimit: Int, version: StdLibVersion) =
    createDApp(
      s"""
         |{-# STDLIB_VERSION ${version.id} #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func issue${actionsLimit}assets() = {
         |    ${(0 until actionsLimit)
        .map(ind => s"""let i$ind = Issue("SponsoredAsset$ind", "SponsoredAsset description", 1000000000000000, 2, true, unit, $ind)""")
        .mkString("\n")}
         |
         |    ${(0 until actionsLimit)
        .map(ind => s"""let issueId$ind = calculateAssetId(i$ind)""")
        .mkString("\n")}
         |
         |    [
         |        ${(0 until actionsLimit)
        .map(ind => s"""BinaryEntry("sponsoredAssetId$ind", issueId$ind),""")
        .mkString("\n")}
         |        ${(0 until actionsLimit).map(ind => s"i$ind").mkString(",")}
         |    ]
         |}
         |
         |@Callable(i)
         |func sponsor${actionsLimit}assets() = [
         |    ${(0 until actionsLimit)
        .map(ind => s"""SponsorFee(this.getBinary("sponsoredAssetId$ind").value(), ${minSponsoredAssetFee.toString})""")
        .mkString(",\n")}
         |]
         |
         |@Callable(i)
         |func sponsor${actionsLimit + 1}assets() = [
         |    ${(0 until actionsLimit)
        .map(ind => s"""SponsorFee(this.getBinary("sponsoredAssetId$ind").value(), ${minSponsoredAssetFee.toString})""")
        .mkString(",\n")},
         |    SponsorFee(this.getBinary("sponsoredAssetId${actionsLimit - 1}").value(), ${minSponsoredAssetFee.toString})
         |]
        """.stripMargin
    )

  def script(actionsLimit: Int, version: StdLibVersion): String = {
    def createIssueParams(asset: Asset) = s""""${asset.name}","${asset.description}",${asset.quantity}, ${asset.decimals},${asset.reissuable},unit"""
    val issueParams                     = createIssueParams(asset)

    s"""
       |{-# STDLIB_VERSION ${version.id} #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |
       |@Callable (i)
       |func issue${actionsLimit}Assets() = {
       |  [
       |    ${(0 until actionsLimit).map(ind => s"Issue($issueParams, $ind)").mkString(",\n")}
       |  ]
       |}
       |
       |@Callable (i)
       |func issue${actionsLimit + 1}Assets() = {
       |  [
       |    ${(1 to actionsLimit + 1).map(ind => s"Issue($issueParams, $ind)").mkString(",\n")}
       |  ]
       |}
       |
       |@Callable (i)
       |func process${actionsLimit + 1}actions(a: ByteVector) = {
       |  [
       |    Reissue(a, 1000, true),
       |    Burn(a, 500),
       |    SponsorFee(a, 1),
       |    ${(1 until (actionsLimit - 1)).map(ind => s"Issue($issueParams, $ind)").mkString(",\n")}
       |  ]
       |}
       |
       """.stripMargin
  }
}
