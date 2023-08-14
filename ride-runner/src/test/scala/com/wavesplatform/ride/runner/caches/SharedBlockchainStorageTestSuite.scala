package com.wavesplatform.ride.runner.caches

import cats.syntax.option.*
import com.google.protobuf.UnsafeByteOperations
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.account.Alias
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.api.{HasGrpc, TestBlockchainApi}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.block.{Block, MicroBlock, SignedMicroBlock}
import com.wavesplatform.protobuf.transaction.PBAmounts.toPBAssetId
import com.wavesplatform.protobuf.transaction.{CreateAliasTransactionData, SetScriptTransactionData, SignedTransaction, Transaction}
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt}
import com.wavesplatform.ride.runner.caches.disk.DefaultDiskCaches
import com.wavesplatform.ride.runner.db.HasTestDb
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Height, IntegerDataEntry, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.{Asset, AssetIdLength}
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

import java.nio.charset.StandardCharsets

class SharedBlockchainStorageTestSuite extends BaseTestSuite with HasTestDb with HasGrpc with HasTestAccounts {
  "SharedBlockchainStorage" - {
    "process with" - {
      "append" - {
        "block" - {
          "ignores unrelated updates" in new Test {
            override val trackAffectedEvents = Seq(mkFilledAppendBlockEvent())

            override def checks(access: Access): Unit = access
              .noTagsAffected()
              .allDataIsUnknown()
          }.runTest()

          "can get the appended data" in new Test {
            override val dependencies        = allDependencies
            override val trackAffectedEvents = Seq(mkFilledAppendBlockEvent())

            override def checks(access: Access): Unit = access
              .allTagsAffected()
              .allDataIsCached()
          }.runTest()

          "updates activated features" in withDb { db =>
            db.batchedReadWrite { implicit rw =>
              val blockchain = SharedBlockchainStorage(
                settings = SharedBlockchainStorage.Settings(
                  blockchain = DefaultBlockchainSettings,
                  blockchainDataCache = InMemBlockchainDataCache.Settings(ConfigMemorySize.ofBytes(1 << 20))
                ),
                allTags = new CacheKeyTags[Tag],
                db = db,
                diskCaches = DefaultDiskCaches(db),
                blockchainApi = testBlockchainApi
              )

              blockchain.activatedFeatures shouldBe DefaultBlockchainSettings.functionalitySettings.preActivatedFeatures

              val newFeatureId = Short.MaxValue
              blockchain.process(
                mkBlockAppendEvent(
                  height = 1,
                  forkNumber = 1,
                  modAppend = x => x.withBlock(x.getBlock.copy(activatedFeatures = Seq(newFeatureId)))
                )
              )

              blockchain.activatedFeatures shouldBe DefaultBlockchainSettings.functionalitySettings.preActivatedFeatures
                .updated(newFeatureId, 1)
            }
          }
        }

        "micro block" - {
          "ignores unrelated updates" in new Test {
            override val preEvents           = Seq(mkEmptyAppendBlockEvent())
            override val trackAffectedEvents = Seq(mkFilledAppendMicroBlockEvent())

            override def checks(access: Access): Unit = access
              .noTagsAffected()
              .allDataIsUnknown()
          }.runTest()

          "can get the appended data" in new Test {
            override val dependencies        = allDependencies
            override val preEvents           = Seq(mkEmptyAppendBlockEvent())
            override val trackAffectedEvents = Seq(mkFilledAppendMicroBlockEvent())

            override def checks(access: Access): Unit = access
              .allTagsAffectedExcept(heightKey)
              .allDataIsCached()
          }.runTest()
        }
      }

      "rollback" - {
        "block" - {
          "ignores unrelated updates" in new Test {
            private val filledAppend = mkFilledAppendBlockEvent(2)

            override val trackAffectedEvents = Seq(
              mkEmptyAppendBlockEvent(1),
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override def checks(access: Access): Unit = access
              .noTagsAffected()
              .allDataIsUnknown()
          }.runTest()

          "can get the restored data" in new Test {
            override val dependencies = allDependencies

            override val preEvents = Seq(mkEmptyAppendBlockEvent(1))

            private val filledAppend = mkFilledAppendBlockEvent(2)
            override val trackAffectedEvents = Seq(
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override def checks(access: Access): Unit = access
              .allTagsAffected()
              .dataIs(
                aliceAccountData = RemoteData.Absence,
                transaction = RemoteData.Absence,
                assetInfo = RemoteData.Absence,
                aliceWavesBalance = RemoteData.Cached(0L),
                bobAssetBalance = RemoteData.Cached(0L),
                aliceLeaseBalance = RemoteData.Cached(LeaseBalance(0L, 0L))
              )
          }.runTest()
        }

        "micro block" - {
          "ignores unrelated updates" in new Test {
            override val preEvents = Seq(mkEmptyAppendBlockEvent())

            private val filledAppend = mkFilledAppendMicroBlockEvent()
            override val trackAffectedEvents = Seq(
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override def checks(access: Access): Unit = access
              .noTagsAffected()
              .allDataIsUnknown()
          }.runTest()

          "can get the restored data" in new Test {
            override val dependencies = allDependencies

            override val preEvents = Seq(mkEmptyAppendBlockEvent())

            private val filledAppend = mkFilledAppendMicroBlockEvent()
            override val trackAffectedEvents = Seq(
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override def checks(access: Access): Unit = access
              .allTagsAffected()
              .dataIs(
                aliceAccountData = RemoteData.Absence,
                transaction = RemoteData.Absence,
                assetInfo = RemoteData.Absence,
                aliceWavesBalance = RemoteData.Cached(0L),
                bobAssetBalance = RemoteData.Cached(0L),
                aliceLeaseBalance = RemoteData.Cached(LeaseBalance(0L, 0L))
              )
          }.runTest()
        }
      }
    }

    "undo after" - {
      "append" - {
        "block" - {
          "ignores unrelated updates" in new Test {
            override val preEvents = Seq(mkEmptyAppendBlockEvent(1))

            private val filledAppend         = mkFilledAppendBlockEvent(2)
            override val trackAffectedEvents = Seq(filledAppend)

            override def checks(access: Access): Unit = {
              access.blockchain.undo(List(filledAppend)) shouldBe empty
              access.allDataIsUnknown()
            }
          }.runTest()

          "can get the restored data" in new Test {
            override val dependencies = allDependencies

            override val preEvents = Seq(mkEmptyAppendBlockEvent(1))

            private val filledAppend         = mkFilledAppendBlockEvent(2)
            override val trackAffectedEvents = Seq(filledAppend)

            override def checks(access: Access): Unit = {
              access.blockchain.undo(List(filledAppend)) shouldBe allTags
              access.allDataIsRestored()
            }
          }.runTest()
        }

        "micro block" - {
          "ignores unrelated updates" in new Test {
            override val preEvents = Seq(mkEmptyAppendBlockEvent(1))

            private val block2               = mkEmptyAppendBlockEvent(2)
            private val microBlock           = mkFilledAppendMicroBlockEvent(2)
            override val trackAffectedEvents = Seq(block2, microBlock)

            override def checks(access: Access): Unit = {
              access.blockchain.undo(List(microBlock, block2)) shouldBe empty // Liquid block
              access.allDataIsUnknown()
            }
          }.runTest()

          "can get the restored data" - {
            "one micro block" in new Test {
              override val dependencies = allDependencies

              override val preEvents = Seq(mkEmptyAppendBlockEvent(1))

              private val block2 = mkFilledAppendBlockEvent(2)
              private val microBlock = mkMicroBlockAppendEvent(
                2,
                1,
                1,
                _.withStateUpdate(
                  StateUpdate.defaultInstance.withDataEntries(Seq(mkDataEntryUpdate(aliceAddr, "x", 1L.some, 2L.some)))
                )
              )
              override val trackAffectedEvents = Seq(block2, microBlock)

              override def checks(access: Access): Unit = {
                access.blockchain.undo(List(microBlock, block2)) shouldBe allTags // Liquid block
                access.allDataIsRestored()
              }
            }.runTest()
          }
        }
      }

      "rollback to" - {
        "block" in new Test {
          override val dependencies = allDependencies

          private val emptyAppend2 = mkEmptyAppendBlockEvent(2)
          private val emptyAppend3 = mkEmptyAppendBlockEvent(3)
          override val trackAffectedEvents = Seq(
            mkFilledAppendBlockEvent(1),
            emptyAppend2,
            emptyAppend3,
            mkRollbackEvent(emptyAppend3)
          )

          override def checks(access: Access): Unit = {
            access.blockchain.undo(List(emptyAppend2)) shouldBe AffectedTags(Set(allDependencies(heightKey)))
            access.allDataIsCached()
            withClue("height") { access.blockchain.height shouldBe 1 }
          }
        }.runTest()

        "micro block" in new Test {
          override val dependencies = allDependencies

          private val emptyAppend1 = mkMicroBlockAppendEvent(height = 2, forkNumber = 1, microBlockNumber = 1)
          private val emptyAppend2 = mkMicroBlockAppendEvent(height = 2, forkNumber = 1, microBlockNumber = 2)

          override val trackAffectedEvents = Seq(
            mkFilledAppendBlockEvent(1),
            mkEmptyAppendBlockEvent(2),
            emptyAppend1,
            emptyAppend2,
            mkRollbackEvent(emptyAppend2)
          )

          override def checks(access: Access): Unit = {
            access.blockchain.undo(List(emptyAppend1)) shouldBe AffectedTags(Set(allDependencies(heightKey)))
            access.allDataIsCached()
            withClue("height") { access.blockchain.height shouldBe 1 }
          }
        }.runTest()
      }
    }
  }

  private val transactionId = TransactionId(ByteStr(Array.fill[Byte](32)(1)))

  private val asset = Asset.IssuedAsset(ByteStr(Array.fill[Byte](AssetIdLength)(2)))
  private val assetDescription = AssetDescription(
    originTransactionId = asset.id,
    issuer = alice.publicKey,
    name = UnsafeByteOperations.unsafeWrap("name".getBytes(StandardCharsets.UTF_8)),
    description = UnsafeByteOperations.unsafeWrap("description".getBytes(StandardCharsets.UTF_8)),
    decimals = 8,
    reissuable = false,
    totalVolume = 1000,
    lastUpdatedAt = Height(0),
    script = None,
    sponsorship = 0,
    nft = false,
    sequenceInBlock = 0,
    issueHeight = Height(1)
  )

  private val accountScript = AccountScriptInfo(
    publicKey = EmptyPublicKey,
    script = Script.fromBase64String("base64:BQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAABXs1wV").explicitGet(),
    verifierComplexity = 4,
    complexitiesByEstimator = Map(1 -> Map.empty)
  )

  private val pbStateUpdate = StateUpdate.defaultInstance
    .withDataEntries(Seq(mkDataEntryUpdate(aliceAddr, "x", none, 1L.some)))
    .withAssets(
      Seq(
        com.wavesplatform.events.StateUpdate.AssetStateUpdate.toPB(
          com.wavesplatform.events.StateUpdate.AssetStateUpdate(
            asset.id,
            before = none,
            after = assetDescription.some
          )
        )
      )
    )
    .withBalances(
      Seq(
        StateUpdate.BalanceUpdate(
          address = aliceAddr.toByteString,
          amountAfter = Amount(toPBAssetId(Asset.Waves), 1L).some
        ),
        StateUpdate.BalanceUpdate(
          address = bobAddr.toByteString,
          amountAfter = Amount(toPBAssetId(asset), 2L).some
        )
      )
    )
    .withLeasingForAddress(
      Seq(
        StateUpdate.LeasingUpdate(
          address = aliceAddr.toByteString,
          inAfter = 4L,
          outAfter = 3L
        )
      )
    )
    .withScripts(
      Seq(
        StateUpdate.ScriptUpdate(
          address = aliceAddr.toByteString,
          after = accountScript.script.bytes().toByteString
        )
      )
    )

  private val pbTransactionIds = Seq(UnsafeByteOperations.unsafeWrap(transactionId.arr))
  private val pbTransactions = Seq(
    SignedTransaction.defaultInstance.withWavesTransaction(
      Transaction.defaultInstance
        .withCreateAlias(CreateAliasTransactionData.defaultInstance.withAlias("foo-alias"))
        .withSenderPublicKey(alice.publicKey.toByteString)
    ),
    SignedTransaction.defaultInstance.withWavesTransaction(
      Transaction.defaultInstance
        .withSenderPublicKey(alice.publicKey.toByteString)
        .withSetScript(SetScriptTransactionData.defaultInstance)
    )
  )

  private def mkEmptyAppendBlockEvent(height: Int = 1) = mkBlockAppendEvent(height, 1)

  private def mkFilledAppendBlockEvent(height: Int = 1) = mkBlockAppendEvent(
    height,
    1,
    // Note, the order of records is wrong. It's okay, because we don't zip transactions with their ids
    _.withStateUpdate(pbStateUpdate).withTransactionIds(pbTransactionIds),
    _.withTransactions(pbTransactions)
  )

  private def mkFilledAppendMicroBlockEvent(height: Int = 1) = mkMicroBlockAppendEvent(
    height,
    1,
    1,
    // Note, the order of records is wrong. It's okay, because we don't zip transactions with their ids
    _.withStateUpdate(pbStateUpdate).withTransactionIds(pbTransactionIds),
    _.withTransactions(pbTransactions)
  )

  private def mkRollbackEvent(append: BlockchainUpdated): BlockchainUpdated = {
    val toHeight = append.height - (append.getAppend.body match {
      case _: Body.Block => 1
      case _             => 0
    })
    BlockchainUpdated()
      .withId(toByteString32(1, toHeight))
      .withHeight(toHeight)
      .withRollback(
        BlockchainUpdated.Rollback(
          `type` = BlockchainUpdated.Rollback.RollbackType.BLOCK,
          removedTransactionIds = append.getAppend.transactionIds,
          removedBlocks = append.getAppend.getBlock.block.toSeq,
          rollbackStateUpdate = mkPbRollbackStateUpdate(append.getAppend.getStateUpdate).some
        )
      )
  }

  private def mkBlockAppendEvent(
      height: Int,
      forkNumber: Int,
      modAppend: BlockchainUpdated.Append => BlockchainUpdated.Append = identity,
      modBlock: Block => Block = identity
  ) = BlockchainUpdated()
    .withId(toByteString32(forkNumber, height))
    .withHeight(height)
    .withUpdate(
      BlockchainUpdated.Update.Append(
        modAppend(
          BlockchainUpdated
            .Append()
            .withBlock(BlockchainUpdated.Append.BlockAppend().withBlock(modBlock(mkPbBlock(height))))
        )
      )
    )

  private def mkMicroBlockAppendEvent(
      height: Int,
      forkNumber: Int,
      microBlockNumber: Int,
      modAppend: BlockchainUpdated.Append => BlockchainUpdated.Append = identity,
      modBlock: MicroBlock => MicroBlock = identity
  ) = BlockchainUpdated()
    .withId(toByteString32(microBlockNumber, forkNumber, height))
    .withHeight(height)
    .withUpdate(
      BlockchainUpdated.Update.Append(
        modAppend(
          BlockchainUpdated
            .Append()
            .withMicroBlock(
              BlockchainUpdated.Append
                .MicroBlockAppend()
                .withMicroBlock(SignedMicroBlock(microBlock = modBlock(mkPbMicroBlock).some))
            )
        )
      )
    )

  private def mkPbRollbackStateUpdate(pbStateUpdate: StateUpdate): StateUpdate = {
    StateUpdate(
      dataEntries = pbStateUpdate.dataEntries.map { x =>
        x.copy(dataEntryBefore = x.dataEntry, dataEntry = x.dataEntryBefore)
      },
      assets = pbStateUpdate.assets.map { x =>
        x.copy(before = x.after, after = x.before)
      },
      balances = pbStateUpdate.balances.map { x =>
        x.copy(
          amountBefore = x.amountAfter.map(_.amount).getOrElse(0L),
          amountAfter = x.amountAfter.map(_.copy(amount = x.amountBefore))
        )
      },
      leasingForAddress = pbStateUpdate.leasingForAddress.map { x =>
        x.copy(
          inBefore = x.inAfter,
          outBefore = x.outAfter,
          inAfter = x.inBefore,
          outAfter = x.outBefore
        )
      },
      scripts = pbStateUpdate.scripts.map { x =>
        x.copy(before = x.after, after = x.before)
      }
    )
  }

  private val testBlockchainApi = new TestBlockchainApi()(monix.execution.schedulers.TestScheduler()) {
    override def getCurrentBlockchainHeight(): Height = Height(1)
    override def getActivatedFeatures(height: Height): Map[Short, Height] =
      DefaultBlockchainSettings.functionalitySettings.preActivatedFeatures.view.mapValues(Height(_)).toMap
  }

  private lazy val heightKey        = CacheKey.Height
  private lazy val accountScriptKey = CacheKey.AccountScript(aliceAddr)
  private lazy val allDependencies: Map[CacheKey, Tag] = Map(
    CacheKey.AccountData(aliceAddr, "x")                    -> 1,
    CacheKey.Transaction(transactionId)                     -> 2,
    heightKey                                               -> 3,
    CacheKey.Alias(Alias.create("foo-alias").explicitGet()) -> 4,
    CacheKey.Asset(asset)                                   -> 5,
    CacheKey.AccountBalance(aliceAddr, Asset.Waves)         -> 6,
    CacheKey.AccountBalance(bobAddr, asset)                 -> 7,
    CacheKey.AccountLeaseBalance(aliceAddr)                 -> 8,
    accountScriptKey                                        -> 9
  )
  private lazy val allTags = AffectedTags(allDependencies.values.toSet)

  private type Tag = Int
  private abstract class Test {
    val dependencies: Map[CacheKey, Tag]            = Map.empty
    val preEvents: Seq[BlockchainUpdated]           = Seq.empty
    val trackAffectedEvents: Seq[BlockchainUpdated] = Seq.empty

    def checks(access: Access): Unit
    def runTest(): Unit = withDb { db =>
      val allTags = new CacheKeyTags[Tag]
      db.directReadWrite { implicit rw =>
        val diskCaches = DefaultDiskCaches(db)

        val blockchain = SharedBlockchainStorage(
          settings = SharedBlockchainStorage.Settings(
            blockchain = DefaultBlockchainSettings,
            blockchainDataCache = InMemBlockchainDataCache.Settings(ConfigMemorySize.ofBytes(1 << 20))
          ),
          allTags = allTags,
          db = db,
          diskCaches = diskCaches,
          blockchainApi = testBlockchainApi
        )

        log.debug("Preparing done, running the test")
        dependencies.foreach(Function.tupled(allTags.addDependent))
        preEvents.foreach(blockchain.process)
        val affectedTags = trackAffectedEvents.foldLeft(AffectedTags.empty[Tag])((r, event) => r ++ blockchain.process(event))
        checks(new Access(blockchain, affectedTags))
      }
    }
  }

  private class Access(val blockchain: SharedBlockchainStorage[Tag], val affectedTags: AffectedTags[Tag]) {
    def get[T <: CacheKey](key: T): RemoteData[T#ValueT] = blockchain.getCachedInMem(key)

    def noTagsAffected(): this.type = withClue("affected tags (noTagsAreAffected)") {
      affectedTags shouldBe empty
      this
    }

    def allTagsAffected(): this.type = allTagsAffectedExcept()

    def allTagsAffectedExcept(keys: CacheKey*): this.type = {
      val expected   = AffectedTags(allTags.xs -- keys.map(allDependencies.apply))
      val sortedDiff = (expected.xs -- affectedTags.xs).toList.sorted
      withClue(s"affected tags, diff={${sortedDiff.mkString(", ")}}") {
        affectedTags shouldBe expected
      }
      this
    }

    def allDataIsUnknown(): this.type = dataIs()

    def allDataIsRestored(): this.type = dataIs(
      aliceAccountData = RemoteData.Absence,
      transaction = RemoteData.Absence,
      assetInfo = RemoteData.Absence,
      aliceWavesBalance = RemoteData.Cached(0L),
      bobAssetBalance = RemoteData.Cached(0L),
      aliceLeaseBalance = RemoteData.Cached(LeaseBalance(0L, 0L)),
      aliceAccountScript = RemoteData.Absence
    )

    def allDataIsCached(): this.type = dataIs(
      aliceAccountData = RemoteData.Cached(IntegerDataEntry("x", 1L)),
      transaction = RemoteData.Cached(Height(1)),
      assetInfo = RemoteData.Cached(WeighedAssetDescription(0, assetDescription)),
      aliceWavesBalance = RemoteData.Cached(1L),
      bobAssetBalance = RemoteData.Cached(2L),
      aliceLeaseBalance = RemoteData.Cached(LeaseBalance(4L, 3L)),
      aliceAccountScript = RemoteData.Cached(
        WeighedAccountScriptInfo(
          scriptInfoWeight = 480,
          accountScriptInfo = AccountScriptInfo(
            publicKey = alice.publicKey,
            script = accountScript.script,
            verifierComplexity = accountScript.verifierComplexity,
            complexitiesByEstimator = accountScript.complexitiesByEstimator
          )
        )
      )
    )

    def dataIs(
        aliceAccountData: RemoteData[CacheKey.AccountData#ValueT] = RemoteData.Unknown,
        transaction: RemoteData[CacheKey.Transaction#ValueT] = RemoteData.Unknown,
        assetInfo: RemoteData[CacheKey.Asset#ValueT] = RemoteData.Unknown,
        aliceWavesBalance: RemoteData[CacheKey.AccountBalance#ValueT] = RemoteData.Unknown,
        bobAssetBalance: RemoteData[CacheKey.AccountBalance#ValueT] = RemoteData.Unknown,
        aliceLeaseBalance: RemoteData[CacheKey.AccountLeaseBalance#ValueT] = RemoteData.Unknown,
        aliceAccountScript: RemoteData[CacheKey.AccountScript#ValueT] = RemoteData.Unknown
    ): this.type = {
      withClue("account data") {
        get(CacheKey.AccountData(aliceAddr, "x")) shouldBe aliceAccountData
      }

      withClue("transaction") {
        get(CacheKey.Transaction(transactionId)) shouldBe transaction
      }

      withClue("asset") {
        get(CacheKey.Asset(asset)) shouldBe assetInfo
      }

      withClue("waves balance") {
        get(CacheKey.AccountBalance(aliceAddr, Asset.Waves)) shouldBe aliceWavesBalance
      }

      withClue("issued asset balance") {
        get(CacheKey.AccountBalance(bobAddr, asset)) shouldBe bobAssetBalance
      }

      withClue("lease balance") {
        get(CacheKey.AccountLeaseBalance(aliceAddr)) shouldBe aliceLeaseBalance
      }

      withClue("account script") {
        get(CacheKey.AccountScript(aliceAddr)) shouldBe aliceAccountScript
      }

      this
    }
  }

}
