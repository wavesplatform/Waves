package com.wavesplatform.http
import com.typesafe.config.ConfigObject
import com.wavesplatform.*
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.{DebugApiRoute, RouteTimeout}
import com.wavesplatform.block.Block
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.{Miner, MinerDebugInfo}
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

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
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
    .copy(
      dbSettings = DomainPresets.TransactionStateSnapshot.dbSettings.copy(storeStateHashes = true),
      restAPISettings = restAPISettings
    )
    .setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> deterministicFinalityActivationHeight)
    .configure(_.copy(generationPeriodLength = 2))

  private val configObject: ConfigObject = settings.config.root()

  private val richAccount = TxHelpers.signer(905)

  override def genesisBalances: Seq[AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 50_000.waves))

  val miner: Miner & MinerDebugInfo = new Miner with MinerDebugInfo {
    override def scheduleMining(blockchain: Option[Blockchain]): Unit = ()

    override def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] = Right(FiniteDuration(0, TimeUnit.SECONDS))

    override def state: MinerDebugInfo.State = MinerDebugInfo.Disabled
  }

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
      miner,
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
          "stateHash"         -> "5b6d80dc02da5d9a76b8f928c0deb18889f25b5515738545532cd164dd70e87c",
          "wavesBalanceHash"  -> "a3766f502f4bba124d9f6fff49adcac44e309bdbc72c437a0607de9c315bcdfa",
          "assetBalanceHash"  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"     -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash" -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"         -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          // Note: "nextCommittedGeneratorsHash" and "committedGeneratorBalancesHash" fields are not present
          "snapshotHash" -> "2ydpHRFSFwcaQ8s9hPyZwcmJFk4cKDNFcb3DRcvyrXZ9",
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

        val secondGenerator = TxHelpers.signer(906)
        val thirdGenerator  = TxHelpers.signer(907)

        val transferTxDefault = TxHelpers.transfer(richAccount, TxHelpers.defaultSigner.toAddress, 1_000.waves)
        val transferTxSecond  = TxHelpers.transfer(richAccount, secondGenerator.toAddress, 1_001.waves)
        val transferTxThird   = TxHelpers.transfer(richAccount, thirdGenerator.toAddress, 1_002.waves)

        domain.appendBlock(transferTxDefault, transferTxSecond, transferTxThird)

        // Fast-forward 1000 blocks
        Range.inclusive(1, 1000).foreach(_ => domain.appendBlock())

        val commitTxDefault = TxHelpers.commitToGeneration(generationPeriodStart = Height(1010), sender = TxHelpers.defaultSigner)
        val commitTxSecond  = TxHelpers.commitToGeneration(generationPeriodStart = Height(1010), sender = secondGenerator)
        val commitTxThird   = TxHelpers.commitToGeneration(generationPeriodStart = Height(1010), sender = thirdGenerator)

        domain.appendBlock(commitTxDefault, commitTxSecond, commitTxThird)
        domain.appendBlock()

        // Assert after commitment, before generation period
        val afterGeneratingBalanceUpdateHeight = domain.blockchain.height - 1
        val afterGeneratingBalanceUpdateHeader = domain.blockchain.blockHeader(afterGeneratingBalanceUpdateHeight).value
        val expectedResponseAfter = Json.obj(
          "stateHash"                      -> "8009c4658ab4df48890a53f309b9fb38ef34e8e6a1524feda363fc607effd735",
          "wavesBalanceHash"               -> "f162260737b87d88e6ff1b5d02dc3e4383e913bd8da8800344a87b7f1420322d",
          "assetBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"                  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"              -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash"    -> "c67c7a5ceb06065b963b0eab3110c264a0af7aabed859b06f1c1359bc029ee72", // Note: non-empty
          "committedGeneratorBalancesHash" -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "snapshotHash"                   -> "97pxuitSr2ftkBJhwq8hM7gQfJ9rmzpT3S37fnybVVrf",
          "blockId"                        -> afterGeneratingBalanceUpdateHeader.id().toString,
          "baseTarget"                     -> afterGeneratingBalanceUpdateHeader.header.baseTarget,
          "height"                         -> afterGeneratingBalanceUpdateHeight,
          "version"                        -> Version.VersionString
        )

        Get(routePath(s"/stateHash/$afterGeneratingBalanceUpdateHeight")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter
        }

        // Fast-forward to generation period change
        // Note: the generating balances are used on this height (parent block for heightOnGenerationPeriod)
        domain.blockchain.generatingBalance(TxHelpers.defaultSigner.toAddress) shouldBe 101803000000L
        domain.blockchain.generatingBalance(secondGenerator.toAddress) shouldBe 90090000000L
        domain.blockchain.generatingBalance(thirdGenerator.toAddress) shouldBe 90190000000L

        domain.appendBlock() // heightOnGenerationPeriod
        domain.appendBlock() // add 1 more block for API requests

        // Assert after commitment, on generation period
        val heightOnGenerationPeriod = domain.blockchain.height - 1
        val headerOnGenerationPeriod = domain.blockchain.blockHeader(heightOnGenerationPeriod).value
        val expectedResponseAfter2 = Json.obj(
          "stateHash"                      -> "a6848babda5748f824c57e5b62aebdad720232583dad4473f02f3f14442a69f9",
          "wavesBalanceHash"               -> "aa737ab08c1d4d8f8aec34e5d3a7f948266da01f6c0d60f349554350374688ab",
          "assetBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"                  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"              -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash"    -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "committedGeneratorBalancesHash" -> "cfeada0229fc3e4c4bc45d852231f189a7b44a9da08ab08f001a7c59b39c52f4", // Note: non-empty
          "snapshotHash"                   -> "23VWD7k7LLLorKRG7245ni4G8LQwpc82StwqPFsz7yW5",
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
