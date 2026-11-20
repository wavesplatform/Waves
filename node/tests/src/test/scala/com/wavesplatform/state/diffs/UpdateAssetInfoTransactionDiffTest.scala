package com.wavesplatform.state.diffs

import com.wavesplatform.TestValues
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.FeeUnit
import com.wavesplatform.state.{Height, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers

class UpdateAssetInfoTransactionDiffTest extends PropSpec with WithDomain {
  private val interval = 3
  private val minFee   = FeeUnit
  private val issuer   = TxHelpers.signer(1)
  private val other    = TxHelpers.signer(2)
  private val settings = RideV6.configure(_.copy(minAssetInfoUpdateInterval = interval))

  private def withIssuer(f: Domain => Unit): Unit = withDomain(settings, AddrWithBalance.enoughBalances(issuer, other))(f)

  private def update(assetId: ByteStr, name: String = "newName", desc: String = "newDesc", fee: Long = minFee) =
    TxHelpers.updateAssetInfo(assetId, name, desc, issuer, fee)

  private def appendEmptyBlocks(d: Domain, n: Int): Unit = (1 to n).foreach(_ => d.appendBlock())

  private def nameAndDescription(d: Domain, assetId: ByteStr) = {
    val info = d.blockchain.assetDescription(IssuedAsset(assetId)).get
    (info.name.toStringUtf8, info.description.toStringUtf8)
  }

  property("only the issuer can update the info of an existing asset") {
    withIssuer { d =>
      val issue = TxHelpers.issue(issuer)
      d.appendBlock(issue)
      appendEmptyBlocks(d, interval)

      d.appendBlockE(TxHelpers.updateAssetInfo(issue.id(), sender = other, fee = minFee)) should produce("Asset was issued by other address")
      d.appendBlockE(update(ByteStr.fill(32)(1))) should produce("Referenced assetId not found")
    }
  }

  property("updating a smart asset requires the script extra fee") {
    withIssuer { d =>
      val issue = TxHelpers.issue(issuer, script = Some(TestCompiler(V6).compileAsset("true")))
      d.appendBlock(issue)
      appendEmptyBlocks(d, interval)

      d.appendBlockE(update(issue.id(), fee = minFee + ScriptExtraFee - 1)) should produce(
        s"Transaction involves 1 scripted assets. Requires $ScriptExtraFee extra fee"
      )
      d.appendAndAssertSucceed(update(issue.id(), fee = minFee + ScriptExtraFee))
    }
  }

  property("rejects a fee below the minimum") {
    withIssuer { d =>
      val issue = TxHelpers.issue(issuer)
      d.appendBlock(issue)
      appendEmptyBlocks(d, interval)

      d.appendBlockE(update(issue.id(), fee = minFee - 1)) should produce(
        s"Fee for UpdateAssetInfoTransaction (${minFee - 1} in WAVES) does not exceed minimal value of $minFee WAVES"
      )
    }
  }

  property("an NFT can be updated") {
    withIssuer { d =>
      val nft = TxHelpers.issue(issuer, amount = 1, decimals = 0, reissuable = false)
      d.appendBlock(nft)
      appendEmptyBlocks(d, interval)

      d.appendAndAssertSucceed(update(nft.id()))
      nameAndDescription(d, nft.id()) shouldBe ("newName", "newDesc")
    }
  }

  property("reissue, burn and setAssetScript do not restart the update interval") {
    withIssuer { d =>
      val script = TestCompiler(V6).compileAsset("true")
      val issue  = TxHelpers.issue(issuer, script = Some(script))
      d.appendBlock(issue)
      val issueHeight = d.blockchain.height
      appendEmptyBlocks(d, interval)

      val smartFee = TestValues.fee + ScriptExtraFee
      d.appendAndAssertSucceed(
        TxHelpers.reissue(issue.asset, issuer, fee = smartFee),
        TxHelpers.burn(issue.asset, sender = issuer, fee = smartFee),
        TxHelpers.setAssetScript(issuer, issue.asset, script)
      )
      d.blockchain.assetDescription(issue.asset).get.lastUpdatedAt shouldBe Height(issueHeight)
      d.appendAndAssertSucceed(update(issue.id(), fee = minFee + ScriptExtraFee))
    }
  }

  property("rolling back an update restores the previous info and lets the asset be updated again") {
    withIssuer { d =>
      val issue = TxHelpers.issue(issuer, name = "name", description = "desc")
      d.appendBlock(issue)
      val issueHeight = d.blockchain.height
      appendEmptyBlocks(d, interval)
      d.appendAndAssertSucceed(update(issue.id()))

      d.rollbackTo(d.blockchain.height - 1)
      nameAndDescription(d, issue.id()) shouldBe ("name", "desc")
      d.blockchain.assetDescription(issue.asset).get.lastUpdatedAt shouldBe Height(issueHeight)

      d.appendAndAssertSucceed(update(issue.id(), "secondName", "secondDesc"))
      nameAndDescription(d, issue.id()) shouldBe ("secondName", "secondDesc")
    }
  }

  property("rolling back the issue counts the interval from the new issue height, not from the discarded update") {
    withIssuer { d =>
      val issue = TxHelpers.issue(issuer)
      d.appendBlock()
      d.appendBlock(issue)
      val issueHeight = d.blockchain.height
      appendEmptyBlocks(d, interval)
      d.appendAndAssertSucceed(update(issue.id()))

      d.rollbackTo(issueHeight - 1)
      d.blockchain.assetDescription(issue.asset) shouldBe None

      d.appendBlock(issue)
      appendEmptyBlocks(d, interval)
      // the discarded update would only allow the next one `interval` blocks after itself
      d.appendAndAssertSucceed(update(issue.id(), "secondName", "secondDesc"))
      nameAndDescription(d, issue.id()) shouldBe ("secondName", "secondDesc")
    }
  }

  property("scripts read the updated name and description") {
    withIssuer { d =>
      val issue = TxHelpers.issue(issuer, script = Some(TestCompiler(V6).compileAsset("true")))
      val dApp = TestCompiler(V6).compileContract(
        s"""
           |@Callable(i)
           |func read() = {
           |  let a = assetInfo(base58'${issue.id()}').value()
           |  [StringEntry("name", a.name), StringEntry("description", a.description)]
           |}
           |""".stripMargin
      )
      d.appendBlock(issue, TxHelpers.setScript(other, dApp))
      appendEmptyBlocks(d, interval)
      d.appendAndAssertSucceed(update(issue.id(), fee = minFee + ScriptExtraFee))

      d.appendAndAssertSucceed(TxHelpers.invoke(other.toAddress, Some("read"), invoker = issuer))
      d.blockchain.accountData(other.toAddress, "name") shouldBe Some(StringDataEntry("name", "newName"))
      d.blockchain.accountData(other.toAddress, "description") shouldBe Some(StringDataEntry("description", "newDesc"))

      val checkInfo = TestCompiler(V6).compileAsset("""this.name == "newName" && this.description == "newDesc"""")
      d.appendAndAssertSucceed(
        TxHelpers.setAssetScript(issuer, issue.asset, checkInfo),
        TxHelpers.transfer(issuer, other.toAddress, 1, issue.asset, fee = TestValues.fee + ScriptExtraFee)
      )
    }
  }
}
