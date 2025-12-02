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
          // Note: "nextCommittedGeneratorsHash" is not present
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

        val transferTx = TxHelpers.transfer(richAccount, TxHelpers.defaultSigner.toAddress, 1_000.waves)
        val commitTx   = TxHelpers.commitToGeneration(generationPeriodStart = Height(8), sender = TxHelpers.defaultSigner)
        domain.appendBlock(transferTx, commitTx)
        domain.appendBlock()

        // Assert after DeterministicFinality feature activation
        val afterFinalityHeight = domain.blockchain.height - 1
        domain.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality, afterFinalityHeight) shouldBe true

        val afterFinalityHeader = domain.blockchain.blockHeader(afterFinalityHeight).value
        val expectedResponseAfter = Json.obj(
          "stateHash"                   -> "db312fac738c0df9903df8e6baa6b3cfee455f2db367ceab6b34f2576c1a3fb2",
          "wavesBalanceHash"            -> "f9b41de484eb180d9b77d2ff88db971bfba7bf19a99857f26c7f5171a43628f4",
          "assetBalanceHash"            -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "dataEntryHash"               -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "accountScriptHash"           -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "assetScriptHash"             -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseBalanceHash"            -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "leaseStatusHash"             -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "sponsorshipHash"             -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "aliasHash"                   -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          "nextCommittedGeneratorsHash" -> "ea94d09632089883a35a7d51ab712c0fade50a16272d6a89f243e37a4f006c17",
          "snapshotHash"                -> "FvSRsH9nGSK2eT3dGsN5Cz2xAhK1WZQvnCF2PqHXj2tv",
          "blockId"                     -> afterFinalityHeader.id().toString,
          "baseTarget"                  -> afterFinalityHeader.header.baseTarget,
          "height"                      -> afterFinalityHeight,
          "version"                     -> Version.VersionString
        )

        Get(routePath(s"/stateHash/last")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter
        }

        Get(routePath(s"/stateHash/$afterFinalityHeight")) ~> route ~> check {
          status shouldBe StatusCodes.OK
          responseAs[JsObject] shouldBe expectedResponseAfter
        }
      }

      "at nonexistent height" in {
        Get(routePath(s"/stateHash/${domain.blockchain.height}")) ~> route ~> check {
          status shouldBe StatusCodes.NotFound
        }
      }
    }
  }
}
