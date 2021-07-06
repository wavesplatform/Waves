package com.wavesplatform.state.patch

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.TxHelpers
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll

class CancelLeasesToDisabledAliasesSpec extends FlatSpec with PathMockFactory with WithDomain with BeforeAndAfterAll {
  val MainnetSettings = {
    import SettingsFromDefaultConfig.blockchainSettings.{functionalitySettings => fs}
    SettingsFromDefaultConfig.copy(
      blockchainSettings = SettingsFromDefaultConfig.blockchainSettings.copy(
        addressSchemeCharacter = 'W',
        functionalitySettings = fs.copy(
          preActivatedFeatures = fs.preActivatedFeatures ++ Map(
            BlockchainFeatures.NG.id               -> 0,
            BlockchainFeatures.SynchronousCalls.id -> 2
          )
        )
      )
    )
  }

  "CancelLeasesToDisabledAliases" should "be applied only once" in withDomain(MainnetSettings) { d =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
    d.appendKeyBlock()
    d.appendMicroBlock(TxHelpers.transfer())
    d.appendMicroBlock(TxHelpers.transfer())
    d.appendMicroBlock(TxHelpers.transfer())
    d.appendKeyBlock()

    val leaseBalance = d.blockchain.leaseBalance(PublicKey(ByteStr(Base58.decode("6NxhjzayDTd52MJL2r6XupGDb7E1xQW7QppSPqo63gsx"))).toAddress)
    leaseBalance.out shouldBe -2562590821L
  }

  it should "be applied on extension apply" in withDomain(MainnetSettings) { d =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
    d.appendBlock()
    d.appendBlock()
    d.appendBlock()

    val leaseBalance = d.blockchain.leaseBalance(PublicKey(ByteStr(Base58.decode("6NxhjzayDTd52MJL2r6XupGDb7E1xQW7QppSPqo63gsx"))).toAddress)
    leaseBalance.out shouldBe -2562590821L
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    AddressScheme.current = new AddressScheme {
      val chainId: Byte = 'W'
    }
  }

  override protected def afterAll(): Unit = {
    AddressScheme.current = new AddressScheme {
      val chainId: Byte = 'T'
    }
    super.afterAll()
  }
}
