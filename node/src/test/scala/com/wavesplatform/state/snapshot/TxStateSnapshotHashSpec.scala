package com.wavesplatform.state.snapshot

import cats.data.Ior
import com.google.common.primitives.{Ints, Longs, UnsignedBytes}
import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.SnapshotOps
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.*
import com.wavesplatform.state.TxStateSnapshotHashBuilder.KeyType
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{AssetIdLength, TxHelpers}

import java.nio.charset.StandardCharsets
import scala.collection.immutable.VectorMap

class TxStateSnapshotHashSpec extends PropSpec with WithDomain {
  val stateHash        = new StateHashBuilder
  private val address1 = TxHelpers.address(1)
  private val address2 = TxHelpers.address(2)
  private val assetId1 = IssuedAsset(ByteStr.fill(AssetIdLength)(1))
  private val assetId2 = IssuedAsset(ByteStr.fill(AssetIdLength)(2))
  private val assetId3 = IssuedAsset(ByteStr.fill(AssetIdLength)(3))
  private val assetId4 = IssuedAsset(ByteStr.fill(AssetIdLength)(4))

  private val orderId      = ByteStr.fill(DigestLength)(5)
  private val volumeAndFee = VolumeAndFee(11, 2)

  private val leaseId      = ByteStr.fill(DigestLength)(6)
  private val leaseDetails = LeaseDetails(TxHelpers.signer(1).publicKey, address2, 1.waves, LeaseDetails.Status.Active, leaseId, 2)

  private val addr1Balance       = 10.waves
  private val addr2Balance       = 20.waves
  private val addr1PortfolioDiff = Portfolio(balance = 2.waves, lease = LeaseBalance(3.waves, 1.waves))
  private val addr2PortfolioDiff = Portfolio(assets = VectorMap(assetId1 -> 123))

  private val addr1Alias1 = Alias(AddressScheme.current.chainId, "addr1Alias1")
  private val addr1Alias2 = Alias(AddressScheme.current.chainId, "addr1Alias2")
  private val addr2Alias  = Alias(AddressScheme.current.chainId, "addr2")

  private val assetInfo1 = NewAssetInfo(
    AssetStaticInfo(assetId1.id, TransactionId(assetId1.id), TxHelpers.signer(1).publicKey, 8, false),
    AssetInfo("test1", "desc1", Height(2)),
    AssetVolumeInfo(true, BigInt(123))
  )
  private val assetInfo2 = NewAssetInfo(
    AssetStaticInfo(assetId2.id, TransactionId(assetId2.id), TxHelpers.signer(1).publicKey, 8, false),
    AssetInfo("test2", "desc2", Height(2)),
    AssetVolumeInfo(true, BigInt(123))
  )
  private val assetInfo3 = NewAssetInfo(
    AssetStaticInfo(assetId3.id, TransactionId(assetId3.id), TxHelpers.signer(1).publicKey, 8, false),
    AssetInfo("test3", "desc3", Height(2)),
    AssetVolumeInfo(true, BigInt(123))
  )
  private val assetInfo4 = NewAssetInfo(
    AssetStaticInfo(assetId4.id, TransactionId(assetId4.id), TxHelpers.signer(1).publicKey, 8, false),
    AssetInfo("test4", "desc4", Height(2)),
    AssetVolumeInfo(true, BigInt(123))
  )
  private val updatedAssetInfo1       = AssetInfo("updTest1", "updDesc1", Height(2))
  private val updatedAssetVolumeInfo1 = AssetVolumeInfo(false, 124)
  private val updatedAssetInfo2       = AssetInfo("updTest2", "updDesc2", Height(2))
  private val updatedAssetVolumeInfo3 = AssetVolumeInfo(false, 125)
  private val sponsorship             = SponsorshipValue(12)

  private val testScript = ScriptCompiler
    .compile(
      """
        |{-# STDLIB_VERSION 2 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |true
        |""".stripMargin,
      ScriptEstimatorV1
    )
    .explicitGet()
    ._1
  private val accountScriptInfo = AccountScriptInfo(TxHelpers.signer(2).publicKey, testScript, 1)
  private val assetScriptInfo   = AssetScriptInfo(testScript, 1)

  private val dataEntry = StringDataEntry("key", "value")

  private val diff = Diff(
    portfolios = Map(address1 -> addr1PortfolioDiff, address2 -> addr2PortfolioDiff),
    issuedAssets = VectorMap(assetId1 -> assetInfo1, assetId2 -> assetInfo2, assetId3 -> assetInfo3, assetId4 -> assetInfo4),
    updatedAssets = Map(
      assetId1 -> Ior.Both(updatedAssetInfo1, updatedAssetVolumeInfo1),
      assetId2 -> Ior.Left(updatedAssetInfo2),
      assetId3 -> Ior.Right(updatedAssetVolumeInfo3)
    ),
    aliases = Map(addr1Alias1 -> address1, addr2Alias -> address2, addr1Alias2 -> address1),
    orderFills = Map(orderId -> volumeAndFee),
    leaseState = Map(leaseId -> leaseDetails),
    scripts = Map(TxHelpers.signer(2).publicKey -> Some(accountScriptInfo)),
    assetScripts = Map(assetId1 -> Some(assetScriptInfo)),
    accountData = Map(address1 -> Map(dataEntry.key -> dataEntry)),
    sponsorship = Map(assetId1 -> sponsorship)
  )

  def hash(bs: Seq[Array[Byte]]): ByteStr = ByteStr(com.wavesplatform.crypto.fastHash(bs.reduce(_ ++ _)))

  property("correctly create transaction state snapshot hash from diff") {
    withDomain(DomainPresets.RideV6, balances = Seq(AddrWithBalance(address1, addr1Balance), AddrWithBalance(address2, addr2Balance))) { d =>
      val snapshot = SnapshotOps.fromDiff(diff, d.blockchain).explicitGet()
      TxStateSnapshotHashBuilder.createHashFromSnapshot(snapshot, None).txStateSnapshotHash shouldBe hash(
        Seq(
          Array(KeyType.WavesBalance.id.toByte) ++ address1.bytes ++ Longs.toByteArray(addr1PortfolioDiff.balance + addr1Balance),
          Array(KeyType.AssetBalance.id.toByte) ++ address2.bytes ++ assetId1.id.arr ++ Longs.toByteArray(addr2PortfolioDiff.assets.head._2),
          Array(KeyType.DataEntry.id.toByte) ++ address1.bytes ++ dataEntry.key.getBytes(StandardCharsets.UTF_8) ++ dataEntry.valueBytes,
          Array(KeyType.AccountScript.id.toByte) ++ address2.bytes ++ accountScriptInfo.script.bytes().arr ++ accountScriptInfo.publicKey.arr ++ Longs
            .toByteArray(accountScriptInfo.verifierComplexity),
          Array(KeyType.AssetScript.id.toByte) ++ assetId1.id.arr ++ testScript.bytes().arr,
          Array(KeyType.LeaseBalance.id.toByte) ++ address1.bytes ++ Longs.toByteArray(addr1PortfolioDiff.lease.in) ++ Longs.toByteArray(
            addr1PortfolioDiff.lease.out
          ),
          Array(KeyType.LeaseStatus.id.toByte) ++ leaseId.arr ++ (if (leaseDetails.isActive) Array(1: Byte) else Array(0: Byte)),
          Array(KeyType.Sponsorship.id.toByte) ++ assetId1.id.arr ++ Longs.toByteArray(sponsorship.minFee),
          Array(KeyType.Alias.id.toByte) ++ address1.bytes ++ addr1Alias1.name.getBytes(StandardCharsets.UTF_8),
          Array(KeyType.Alias.id.toByte) ++ address1.bytes ++ addr1Alias2.name.getBytes(StandardCharsets.UTF_8),
          Array(KeyType.Alias.id.toByte) ++ address2.bytes ++ addr2Alias.name.getBytes(StandardCharsets.UTF_8),
          Array(KeyType.VolumeAndFee.id.toByte) ++ orderId.arr ++ Longs.toByteArray(volumeAndFee.volume) ++ Longs.toByteArray(volumeAndFee.fee),
          Array(KeyType.AssetStatic.id.toByte) ++ assetId1.id.arr ++ assetInfo1.static.issuer.arr ++
            Array(assetInfo1.static.decimals.toByte) ++ (if (assetInfo1.static.nft) Array(1: Byte) else Array(0: Byte)),
          Array(KeyType.AssetStatic.id.toByte) ++ assetId2.id.arr ++ assetInfo2.static.issuer.arr ++
            Array(assetInfo2.static.decimals.toByte) ++ (if (assetInfo2.static.nft) Array(1: Byte) else Array(0: Byte)),
          Array(KeyType.AssetStatic.id.toByte) ++ assetId3.id.arr ++ assetInfo3.static.issuer.arr ++
            Array(assetInfo3.static.decimals.toByte) ++ (if (assetInfo3.static.nft) Array(1: Byte) else Array(0: Byte)),
          Array(KeyType.AssetStatic.id.toByte) ++ assetId4.id.arr ++ assetInfo4.static.issuer.arr ++
            Array(assetInfo4.static.decimals.toByte) ++ (if (assetInfo4.static.nft) Array(1: Byte) else Array(0: Byte)),
          Array(KeyType.AssetVolume.id.toByte) ++ assetId1.id.arr ++
            (if (updatedAssetVolumeInfo1.isReissuable) Array(1: Byte) else Array(0: Byte)) ++ snapshot.assetVolumes(assetId1).volume.toByteArray,
          Array(KeyType.AssetVolume.id.toByte) ++ assetId2.id.arr ++
            (if (assetInfo2.volume.isReissuable) Array(1: Byte) else Array(0: Byte)) ++ snapshot.assetVolumes(assetId2).volume.toByteArray,
          Array(KeyType.AssetVolume.id.toByte) ++ assetId3.id.arr ++
            (if (updatedAssetVolumeInfo3.isReissuable) Array(1: Byte) else Array(0: Byte)) ++ snapshot.assetVolumes(assetId3).volume.toByteArray,
          Array(KeyType.AssetVolume.id.toByte) ++ assetId4.id.arr ++
            (if (assetInfo4.volume.isReissuable) Array(1: Byte) else Array(0: Byte)) ++ snapshot.assetVolumes(assetId4).volume.toByteArray,
          Array(
            KeyType.AssetNameDescription.id.toByte
          ) ++ assetId1.id.arr ++ updatedAssetInfo1.name.toByteArray ++ updatedAssetInfo1.description.toByteArray ++ Ints.toByteArray(
            updatedAssetInfo1.lastUpdatedAt
          ),
          Array(
            KeyType.AssetNameDescription.id.toByte
          ) ++ assetId2.id.arr ++ updatedAssetInfo2.name.toByteArray ++ updatedAssetInfo2.description.toByteArray ++ Ints.toByteArray(
            updatedAssetInfo2.lastUpdatedAt
          ),
          Array(
            KeyType.AssetNameDescription.id.toByte
          ) ++ assetId3.id.arr ++ assetInfo3.dynamic.name.toByteArray ++ assetInfo3.dynamic.description.toByteArray ++ Ints.toByteArray(
            assetInfo3.dynamic.lastUpdatedAt
          ),
          Array(
            KeyType.AssetNameDescription.id.toByte
          ) ++ assetId4.id.arr ++ assetInfo4.dynamic.name.toByteArray ++ assetInfo4.dynamic.description.toByteArray ++ Ints.toByteArray(
            assetInfo4.dynamic.lastUpdatedAt
          )
        ).sorted((x: Array[Byte], y: Array[Byte]) => UnsignedBytes.lexicographicalComparator().compare(x, y))
      )
    }
  }

  property("correctly compute hash using previous value") {
    val txStateHash = TxStateSnapshotHashBuilder.Result(ByteStr.fill(DigestLength)(1))
    val prevHash    = ByteStr.fill(DigestLength)(2)

    txStateHash.createHash(prevHash) shouldBe hash(Seq(prevHash.arr ++ txStateHash.txStateSnapshotHash.arr))
  }
}
