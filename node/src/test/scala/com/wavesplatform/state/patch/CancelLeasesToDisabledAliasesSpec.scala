package com.wavesplatform.state.patch

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.TxHelpers
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll

class CancelLeasesToDisabledAliasesSpec extends FlatSpec with PathMockFactory with WithDomain with BeforeAndAfterAll {
  val MainnetSettings: WavesSettings = {
    import SettingsFromDefaultConfig.blockchainSettings.{functionalitySettings => fs}
    SettingsFromDefaultConfig.copy(
      blockchainSettings = SettingsFromDefaultConfig.blockchainSettings.copy(
        addressSchemeCharacter = 'W',
        functionalitySettings = fs.copy(
          preActivatedFeatures = fs.preActivatedFeatures ++ Map(
            BlockchainFeatures.NG.id               -> 0,
            BlockchainFeatures.SmartAccounts.id    -> 0,
            BlockchainFeatures.SynchronousCalls.id -> 2
          )
        )
      )
    )
  }

  "CancelLeasesToDisabledAliases" should "be applied only once" in withDomain(MainnetSettings) { d =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
    testLeaseBalance(d).out shouldBe 0L

    d.appendKeyBlock()
    testLeaseBalance(d).out shouldBe -2562590821L

    d.appendMicroBlock(TxHelpers.transfer())
    d.appendMicroBlock(TxHelpers.transfer())
    d.appendMicroBlock(TxHelpers.transfer())
    d.appendKeyBlock()
    testLeaseBalance(d).out shouldBe -2562590821L
  }

  it should "be applied on extension apply" in withDomain(MainnetSettings) { d =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
    testLeaseBalance(d).out shouldBe 0L
    d.appendBlock()
    testLeaseBalance(d).out shouldBe -2562590821L
    d.appendBlock()
    testLeaseBalance(d).out shouldBe -2562590821L
    d.appendBlock()
    testLeaseBalance(d).out shouldBe -2562590821L
  }

  private def testLeaseBalance(d: Domain) = {
    d.blockchain.leaseBalance(PublicKey(ByteStr(Base58.decode("6NxhjzayDTd52MJL2r6XupGDb7E1xQW7QppSPqo63gsx"))).toAddress)
  }
}
