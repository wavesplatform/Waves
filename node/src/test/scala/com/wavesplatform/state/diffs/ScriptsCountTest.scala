package com.wavesplatform.state.diffs

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state._
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptsCountTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id  -> 0,
      BlockchainFeatures.SmartAssets.id    -> 0,
      BlockchainFeatures.Ride4DApps.id     -> 0,
      BlockchainFeatures.DataTransaction.id     -> 0,
      BlockchainFeatures.MassTransfer.id     -> 0,
      BlockchainFeatures.FeeSponsorship.id -> 0
    ))

  val allAllowed = ExprScript(
    FUNCTION_CALL(FunctionHeader.Native(FunctionIds.GT_LONG), List(GETTER(REF("tx"), "fee"), CONST_LONG(-1)))
  ).explicitGet()


  property("check scripts run count") {
    forAll(for {
      master  <- accountGen
      acc <- accountGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      fee         = 1000000000L
      setContract = SetScriptTransaction.selfSigned(master, Some(allAllowed), fee, ts).explicitGet()
      resetContract = SetScriptTransaction.selfSigned(master, Some(allAllowed), fee, ts+1).explicitGet()
      (_, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      issueSp = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, assetName, description, quantity, decimals, true, None, iFee, timestamp)
        .explicitGet()
      sponsorTx = SponsorFeeTransaction.selfSigned(master, IssuedAsset(issueSp.id()), Some(1), fee, timestamp).explicitGet()
      burnSp = BurnTransactionV2.selfSigned(AddressScheme.current.chainId, master,IssuedAsset(issueSp.id()) , 1, fee, timestamp).explicitGet()
      reissueSp = ReissueTransactionV2.selfSigned(AddressScheme.current.chainId, master,IssuedAsset(issueSp.id()) , 1, true, fee, timestamp).explicitGet()
      issueScr = IssueTransactionV2
        .selfSigned(AddressScheme.current.chainId, master, assetName, description, quantity, decimals, true, Some(allAllowed), iFee, timestamp)
        .explicitGet()
      burnScr = BurnTransactionV2.selfSigned(AddressScheme.current.chainId, master,IssuedAsset(issueScr.id()) , 1, fee, timestamp).explicitGet()
      reissueScr = ReissueTransactionV2.selfSigned(AddressScheme.current.chainId, master,IssuedAsset(issueScr.id()) , 1, true, fee, timestamp).explicitGet()
      assetScript =  SetAssetScriptTransaction
       .create(AddressScheme.current.chainId, master, IssuedAsset(issueScr.id()), Some(allAllowed), fee, timestamp, Proofs.empty)
       .explicitGet()
      data = DataTransaction.selfSigned(master, List(BooleanDataEntry("q", true)), 15000000, timestamp).explicitGet()
      tr1 = TransferTransactionV2
        .selfSigned(Waves, master, acc, 4, timestamp, Waves, fee, ByteStr(Array()))
        .explicitGet()
      tr2 = TransferTransactionV2
        .selfSigned(IssuedAsset(issueScr.id()), master, acc, 4, timestamp, Waves, fee, ByteStr(Array()))
        .explicitGet()
      mt1 = MassTransferTransaction.selfSigned(Waves, master, List(ParsedTransfer(acc, 1)), timestamp, fee, ByteStr(Array())).explicitGet()
      mt2 = MassTransferTransaction.selfSigned(IssuedAsset(issueScr.id()), master, List(ParsedTransfer(acc, 1)), timestamp, fee, ByteStr(Array())).explicitGet()
    } yield {
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(
        Seq(
          setContract,
          issueSp,  // 1
          sponsorTx, // 1
          issueScr,  // 1
          burnSp,    // 1
          burnScr,   // 2
          reissueSp, // 1
          reissueScr, // 2
          resetContract, // 1
          assetScript,  // 2
          data, // 1
          tr1,   // 1
          tr2,   // 2
          mt1,  // 1
          mt2  // 2
          )), fs) {
        case (blockDiff, newState) =>
          blockDiff.scriptsRun shouldBe 19
      }
      }) { x => x }
  }
}
