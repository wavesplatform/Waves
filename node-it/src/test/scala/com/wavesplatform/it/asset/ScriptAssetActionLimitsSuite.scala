package com.wavesplatform.it.asset

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.features.BlockchainFeatures.{RideV6, SynchronousCalls}
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.test.*

trait ScriptAssetActionLimitsSuite extends BaseFreeSpec {

  def createDApp(script: String, address: KeyPair = miner.generateKeyPair()): KeyPair

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((SynchronousCalls.id, 0), (RideV6.id, 0)))
      .buildNonConflicting()

  protected val initialWavesBalance: Long  = 1000.waves
  protected val minSponsoredAssetFee: Long = 1001
  protected val asset: Asset               = Asset("Simple", "ReissuableAsset", "description", 100000000, reissuable = true, 3, 0)

  protected def createSponsorFeeDApp(actionsLimit: Int, version: StdLibVersion): KeyPair =
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

  protected def script(actionsLimit: Int, version: StdLibVersion): String = {
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
