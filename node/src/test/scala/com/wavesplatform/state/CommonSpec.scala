package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.GenesisTransaction

class CommonSpec extends FreeSpec with WithDomain {
  private val time          = new TestTime
  private def nextTs        = time.getTimestamp()
  private val AssetIdLength = 32

  private def genesisBlock(genesisTs: Long, address: Address, initialBalance: Long) = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](SignatureLength)(0)),
    Seq(GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet())
  )

  "Common Conditions" - {
    "Zero balance of absent asset" in forAll(accountGen, positiveLongGen, byteArrayGen(AssetIdLength)) {
      case (sender, initialBalance, assetId) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender.toAddress, initialBalance))
          d.balance(sender.toAddress, IssuedAsset(ByteStr(assetId))) shouldEqual 0L
        }
    }
  }
}
