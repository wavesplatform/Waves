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

        // Fast-forward to DeterministicFinality feature activation
        val currentHeight = domain.blockchain.height
        val targetHeight  = deterministicFinalityActivationHeight
        if (currentHeight < targetHeight) {
          val blocksToAdd = targetHeight - currentHeight
          Range.inclusive(0, blocksToAdd).foreach(_ => domain.appendBlock())
        }

        val secondGenerator = TxHelpers.signer(906)
        val thirdGenerator  = TxHelpers.signer(907)

        val transferTxDefault = TxHelpers.transfer(richAccount, TxHelpers.defaultSigner.toAddress, 1_000.waves)
        val transferTxSecond  = TxHelpers.transfer(richAccount, secondGenerator.toAddress, 1_001.waves)
        val transferTxThird   = TxHelpers.transfer(richAccount, thirdGenerator.toAddress, 1_002.waves)

        val commitTxDefault = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = TxHelpers.defaultSigner)
        val commitTxSecond  = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = secondGenerator)
        val commitTxThird   = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = thirdGenerator)

        domain.appendBlock(
          transferTxDefault,
          transferTxSecond,
          transferTxThird,
          commitTxDefault,
          commitTxSecond,
          commitTxThird
        )
        domain.appendBlock()

        // Assert after DeterministicFinality feature activation
        val afterFinalityHeight = domain.blockchain.height - 1
        domain.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality, afterFinalityHeight) shouldBe true

        val afterFinalityHeader = domain.blockchain.blockHeader(afterFinalityHeight).value
        val expectedResponseAfter = Json.obj(
          "stateHash"                      -> "3510239200f1052df4ba4d475f730ba7d0994317e86f711ecf856d7d8e768f90",
          "wavesBalanceHash"               -> "87064c16ed748aee07d591e1abd6d03078ebaafc6ef3cdfe24eaf190f6433326",
          "assetBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"                  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"              -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash"    -> "c67c7a5ceb06065b963b0eab3110c264a0af7aabed859b06f1c1359bc029ee72",
          "committedGeneratorBalancesHash" -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "snapshotHash"                   -> "7vb6RK1Yajua8XuLzTYnkLHfRxy7rYa1YJQgGM74PFCM",
          "blockId"                        -> afterFinalityHeader.id().toString,
          "baseTarget"                     -> afterFinalityHeader.header.baseTarget,
          "height"                         -> afterFinalityHeight,
          "version"                        -> Version.VersionString
        )

        Get(routePath(s"/stateHash/last")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter
        }

        Get(routePath(s"/stateHash/$afterFinalityHeight")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter
        }

        // Fast-forward to generation period change
        domain.appendBlock()

        val heightOnGenerationPeriod = domain.blockchain.height - 1
        val headerOnGenerationPeriod = domain.blockchain.blockHeader(heightOnGenerationPeriod).value

        val expectedResponseAfter2 = Json.obj(
          "stateHash"                      -> "99a076718d2f2fa39e12c6635c427bdf3d80baf924cd815f6a9ebcc555338bd2",
          "wavesBalanceHash"               -> "c4c01e5f091290c8f068e80b24797d2ec763ec46dfb804cc63991788abc94525",
          "assetBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"                  -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"              -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"                -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash"    -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "committedGeneratorBalancesHash" -> "19ea4a516c66775ea1f648d71f6b8fa227e8b0c1a0c9203f82c33b89c4e759b5",
          "snapshotHash"                   -> "HYnC3XFMcKWDWHs8conQSUJe49jk6XVWD34MfizzS6Tq",
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
