package com.wavesplatform.http

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.RewardApiRoute
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.transaction.TxHelpers
import play.api.libs.json.JsValue

class RewardApiRouteSpec extends RouteSpec("/blockchain") with WithDomain {

  val daoAddress: Address        = TxHelpers.address(100)
  val xtnBuybackAddress: Address = TxHelpers.address(101)

  val settingsWithoutAddresses: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = None, xtnBuybackAddress = None)
    )
  )
  val settingsWithOnlyDaoAddress: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = None)
    )
  )
  val settingsWithOnlyXtnBuybackAddress: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings.copy(xtnBuybackAddress = Some(xtnBuybackAddress.toString), daoAddress = None)
    )
  )
  val settingsWithBothAddresses: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings
        .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString))
    )
  )

  routePath("/rewards") in {
    checkWithSettings(settingsWithoutAddresses)
    checkWithSettings(settingsWithOnlyDaoAddress)
    checkWithSettings(settingsWithOnlyXtnBuybackAddress)
    checkWithSettings(settingsWithBothAddresses)
  }

  routePath("/rewards/{height}") in {
    checkWithSettings(settingsWithoutAddresses, Some(1))
    checkWithSettings(settingsWithOnlyDaoAddress, Some(1))
    checkWithSettings(settingsWithOnlyXtnBuybackAddress, Some(1))
    checkWithSettings(settingsWithBothAddresses, Some(1))
  }

  private def checkWithSettings(settings: WavesSettings, height: Option[Int] = None) =
    withDomain(settings) { d =>
      val route = RewardApiRoute(d.blockchain).route

      d.appendBlock()

      val pathSuffix = height.fold("")(h => s"/$h")

      Get(routePath(s"/rewards$pathSuffix")) ~> route ~> check {
        responseAs[JsValue] should matchJson(expectedResponse(d))
      }
    }

  private def expectedResponse(d: Domain) =
    s"""
       |{
       |  "height" : 1,
       |  "totalWavesAmount" : ${d.blockchain.settings.genesisSettings.initialBalance + d.blockchain.settings.rewardsSettings.initial},
       |  "currentReward" : ${d.blockchain.settings.rewardsSettings.initial},
       |  "minIncrement" : ${d.blockchain.settings.rewardsSettings.minIncrement},
       |  "term" : ${d.blockchain.settings.rewardsSettings.term},
       |  "nextCheck" : ${d.blockchain.settings.rewardsSettings.nearestTermEnd(0, 1)},
       |  "votingIntervalStart" : ${d.blockchain.settings.rewardsSettings
      .nearestTermEnd(0, 1) - d.blockchain.settings.rewardsSettings.votingInterval + 1},
       |  "votingInterval" : ${d.blockchain.settings.rewardsSettings.votingInterval},
       |  "votingThreshold" : ${d.blockchain.settings.rewardsSettings.votingInterval / 2 + 1},
       |  "votes" : {
       |    "increase" : 0,
       |    "decrease" : 0
       |  },
       |  "daoAddress" : ${d.blockchain.settings.functionalitySettings.daoAddress.fold("null")(addr => s"\"$addr\"")},
       |  "xtnBuybackAddress" : ${d.blockchain.settings.functionalitySettings.xtnBuybackAddress.fold("null")(addr => s"\"$addr\"")}
       |}
       |""".stripMargin
}
