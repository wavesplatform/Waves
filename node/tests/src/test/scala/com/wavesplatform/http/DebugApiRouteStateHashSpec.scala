package com.wavesplatform.http
import com.typesafe.config.ConfigObject
import com.wavesplatform.*
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.{DebugApiRoute, RouteTimeout}
import com.wavesplatform.block.Block
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.TestMiner
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SharedSchedulerMixin
import monix.eval.Task
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.scalatest.OptionValues
import play.api.libs.json.{JsObject, Json}

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*

class DebugApiRouteStateHashSpec
    extends RouteSpec("/debug")
    with RestAPISettingsHelper
    with TestWallet
    with NTPTime
    with SharedDomain
    with OptionValues
    with SharedSchedulerMixin {

  private lazy val deterministicFinalityActivationHeight = 5

  override def settings: WavesSettings = DomainPresets.TransactionStateSnapshot
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .copy(
      dbSettings = DomainPresets.TransactionStateSnapshot.dbSettings.copy(storeStateHashes = true),
      restAPISettings = restAPISettings
    )
    .setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> deterministicFinalityActivationHeight)
    .configure(_.copy(generationPeriodLength = 2))

  private val configObject: ConfigObject = settings.config.root()

  private val secondGenerator = TxHelpers.signer(906)
  private val thirdGenerator  = TxHelpers.signer(907)

  override def genesisBalances: Seq[AddrWithBalance] = Seq(
    AddrWithBalance(TxHelpers.defaultSigner.toAddress, 10_000.waves),
    AddrWithBalance(secondGenerator.toAddress, 11_000.waves),
    AddrWithBalance(thirdGenerator.toAddress, 12_000.waves)
  )

  val block: Block = TestBlock.create(Nil).block

  val debugApiRoute: DebugApiRoute =
    DebugApiRoute(
      settings,
      ntpTime,
      domain.blockchain,
      domain.wallet,
      domain.accountsApi,
      domain.transactionsApi,
      domain.assetsApi,
      PeerDatabase.NoOp,
      new ConcurrentHashMap(),
      (blockId, _) => Task(domain.blockchain.removeAfter(blockId).map(_ => ())),
      domain.utxPool,
      TestMiner.SafelyDisabled,
      null,
      null,
      null,
      null,
      configObject,
      domain.rocksDBWriter,
      new RouteTimeout(60.seconds)(using sharedScheduler),
      sharedScheduler
    )

  private val route = seal(debugApiRoute.route)

  routePath("/stateHash") - {
    "works" - {
      "before and after DeterministicFinality activation" in {
        // Append first block to be able to request stateHash
        domain.appendBlock()

        // Assert after DeterministicFinality feature activation
        domain.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality, domain.blockchain.height) shouldBe false
        val beforeFinalityHeight = domain.blockchain.height - 1
        val beforeFinalityHeader = domain.blockchain.blockHeader(beforeFinalityHeight).value
        val expectedResponseBefore = Json.obj(
          "stateHash"         -> "9a4cde77f9ae2207e23e94457dceb4fa8826a0c8a56466c1e28d83d35e087588",
          "wavesBalanceHash"  -> "6bb9f756054493145441eb34bb09d0c26ae78ab09e25c9061b11dbac756b9b65",
          "assetBalanceHash"  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"     -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash" -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"         -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          // Note: "nextCommittedGeneratorsHash" and "committedGeneratorBalancesHash" fields are not present
          "snapshotHash" -> "GfGLptLRk1pw9fcYL3qyCzN7XWsnqftkGmX4zsWnHvoD",
          "blockId"      -> beforeFinalityHeader.id().toString,
          "baseTarget"   -> beforeFinalityHeader.header.baseTarget,
          "height"       -> beforeFinalityHeight,
          "version"      -> Version.VersionString
        )

        Get(routePath(s"/stateHash/$beforeFinalityHeight")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseBefore
        }

        Get(routePath(s"/stateHash/last")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseBefore
        }

        // Fast-forward to DeterministicFinality feature activation
        val currentHeight = domain.blockchain.height
        val targetHeight  = deterministicFinalityActivationHeight
        if (currentHeight < targetHeight) {
          val blocksToAdd = targetHeight - currentHeight
          Range.inclusive(0, blocksToAdd).foreach(_ => domain.appendBlock())
        }

        // Assert after DeterministicFinality feature activation
        val afterFinalityHeight = domain.blockchain.height - 1
        domain.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality, afterFinalityHeight) shouldBe true

        val commitTxDefault = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = TxHelpers.defaultSigner)
        val commitTxSecond  = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = secondGenerator)
        val commitTxThird   = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = thirdGenerator)
        domain.appendBlock(commitTxDefault, commitTxSecond, commitTxThird)
        domain.appendBlock()

        // Assert after commitment, before generation period
        val afterGeneratingBalanceUpdateHeight = domain.blockchain.height - 1
        val afterGeneratingBalanceUpdateHeader = domain.blockchain.blockHeader(afterGeneratingBalanceUpdateHeight).value
        val expectedResponseAfter = Json.obj(
          "stateHash"                      -> "b0a55af162af037a8980b2c284bdb8faf3e08a470f768a15e862633a93e2857d",
          "wavesBalanceHash"               -> "b00aadcea779b68ff76fe9cfac28f48786a299a856cd9fa42fc82bcab5149400",
          "assetBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"                  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"              -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash"    -> "0c081cdc089066b9679d6a6abe7e30f415dca425a8d4442062748a73013a5aa9", // Note: non-empty
          "committedGeneratorBalancesHash" -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "snapshotHash"                   -> "88DmToc9xC1dtxk2QYbx5Y42YwA6woGC9PFJ6DYioCqr",
          "blockId"                        -> afterGeneratingBalanceUpdateHeader.id().toString,
          "baseTarget"                     -> afterGeneratingBalanceUpdateHeader.header.baseTarget,
          "height"                         -> afterGeneratingBalanceUpdateHeight,
          "version"                        -> Version.VersionString
        )

        Get(routePath(s"/stateHash/$afterGeneratingBalanceUpdateHeight")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter
        }

        // Note: the generating balances are used on this height (parent block for heightOnGenerationPeriod)
        domain.blockchain.generatingBalance(TxHelpers.defaultSigner.toAddress) shouldBe 991202000000L
        domain.blockchain.generatingBalance(secondGenerator.toAddress) shouldBe 1089990000000L
        domain.blockchain.generatingBalance(thirdGenerator.toAddress) shouldBe 1189990000000L

        // Fast-forward to generation period change
        domain.appendBlock() // heightOnGenerationPeriod
        domain.appendBlock() // add 1 more block for API requests

        // Assert after commitment, on generation period
        val heightOnGenerationPeriod = domain.blockchain.height - 1
        val headerOnGenerationPeriod = domain.blockchain.blockHeader(heightOnGenerationPeriod).value
        val expectedResponseAfter2 = Json.obj(
          "stateHash"                      -> "6bca7f4bd0390ad787d11d45048f1b6306d9492d8419dda28409683836b2ebf5",
          "wavesBalanceHash"               -> "0bd486480e3b07b95227f0c5e6d132aff5c98491d2f60b97c4cc43a8e7aa375b",
          "assetBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"                  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"              -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash"    -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "committedGeneratorBalancesHash" -> "ea4322a8f09a9d010956932ebde7b98a703f5679b85df9c29a44d0de254f705e", // Note: non-empty
          "snapshotHash"                   -> "rNcJukRBu5xcLBKPzdK9qyTgRsPdmhYMbsaSBSXiY49",
          "blockId"                        -> headerOnGenerationPeriod.id().toString,
          "baseTarget"                     -> headerOnGenerationPeriod.header.baseTarget,
          "height"                         -> heightOnGenerationPeriod,
          "version"                        -> Version.VersionString
        )

        Get(routePath(s"/stateHash/last")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter2
        }
      }
    }
  }
}
