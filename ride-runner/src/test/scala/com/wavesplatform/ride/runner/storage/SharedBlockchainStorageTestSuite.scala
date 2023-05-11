package com.wavesplatform.ride.runner.storage

import cats.syntax.option.*
import com.google.protobuf.{ByteString, UnsafeByteOperations}
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
import com.wavesplatform.protobuf.transaction.{CreateAliasTransactionData, SignedTransaction, Transaction}
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt}
import com.wavesplatform.ride.runner.db.ReadWrite
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, HasDb}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Height, IntegerDataEntry, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.{Asset, AssetIdLength}
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

import java.nio.charset.StandardCharsets

class SharedBlockchainStorageTestSuite extends BaseTestSuite with HasDb with HasGrpc with HasTestAccounts {
  "SharedBlockchainStorageNewTest" - {
    "process with" - {
      "append" - {
        "block" - {
          "ignores unrelated updates" in new Test {
            override val trackAffectedEvents = Seq(mkFilledAppendBlockEvent())

            override def doOnComplete(access: Access): Unit = {
              noTagsAreAffected(access)
              allDataIsUnknownCheck(access)
            }
          }.runTest()

          "can get the appended data" in new Test {
            override val trackAffectedEvents = Seq(mkFilledAppendBlockEvent())
            override val dependencies        = allDependencies

            override def doOnComplete(access: Access): Unit = {
              allTagsAreAffected(access)
              allDataIsCachedCheck(access)
            }
          }.runTest()

          "updates activated features" in withDb { db =>
            db.readWrite { implicit rw =>
              val blockchain = SharedBlockchainStorage[Tag](
                settings = SharedBlockchainStorage.Settings(
                  blockchain = DefaultBlockchainSettings,
                  commonCache = CommonCache.Settings(ConfigMemorySize.ofBytes(1 << 20))
                ),
                db = db,
                persistentCaches = DefaultPersistentCaches(db),
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

            override def doOnComplete(access: Access): Unit = {
              noTagsAreAffected(access)
              allDataIsUnknownCheck(access)
            }
          }.runTest()

          "can get the appended data" in new Test {
            override val preEvents           = Seq(mkEmptyAppendBlockEvent())
            override val trackAffectedEvents = Seq(mkFilledAppendMicroBlockEvent())
            override val dependencies        = allDependencies

            override def doOnComplete(access: Access): Unit = {
              allTagsExceptHeightAreAffected(access)
              allDataIsCachedCheck(access)
            }

            def allTagsExceptHeightAreAffected(access: Access): Unit = withClue("affected tags") {
              access.affectedTags shouldBe AffectedTags((1 to 9).toSet - dependencies(CacheKey.Height))
            }
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

            override def doOnComplete(access: Access): Unit = {
              noTagsAreAffected(access)
              allDataIsUnknownCheck(access)
            }
          }.runTest()

          "can get the restored data" in new Test {
            private val filledAppend = mkFilledAppendBlockEvent(2)

            override val preEvents = Seq(mkEmptyAppendBlockEvent(1))
            override val trackAffectedEvents = Seq(
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override val dependencies = allDependencies

            override def doOnComplete(access: Access): Unit = {
              allTagsAreAffected(access)

              withClue("account data") {
                access.get(CacheKey.AccountData(aliceAddr, "x")) shouldBe RemoteData.Absence
              }

              withClue("transaction") {
                access.get(CacheKey.Transaction(transactionId)) shouldBe RemoteData.Unknown
              }

              withClue("asset") {
                access.get(CacheKey.Asset(asset)) shouldBe RemoteData.Absence
              }

              withClue("waves balance") {
                access.get(CacheKey.AccountBalance(aliceAddr, Asset.Waves)) shouldBe RemoteData.Cached(0L)
              }

              withClue("issued asset balance") {
                access.get(CacheKey.AccountBalance(bobAddr, asset)) shouldBe RemoteData.Cached(0L)
              }

              withClue("lease balance") {
                access.get(CacheKey.AccountLeaseBalance(aliceAddr)) shouldBe RemoteData.Cached(LeaseBalance(0L, 0L))
              }

              withClue("account script") {
                access.get(CacheKey.AccountScript(aliceAddr)) shouldBe RemoteData.Absence
              }
            }
          }.runTest()
        }

        "micro block" - {
          "ignores unrelated updates" in new Test {
            private val filledAppend = mkFilledAppendMicroBlockEvent()

            override val preEvents = Seq(mkEmptyAppendBlockEvent())
            override val trackAffectedEvents = Seq(
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override def doOnComplete(access: Access): Unit = {
              noTagsAreAffected(access)
              allDataIsUnknownCheck(access)
            }
          }.runTest()

          "can get the restored data" in new Test {
            private val filledAppend = mkFilledAppendMicroBlockEvent()

            override val preEvents = Seq(mkEmptyAppendBlockEvent())
            override val trackAffectedEvents = Seq(
              filledAppend,
              mkRollbackEvent(filledAppend)
            )

            override val dependencies = allDependencies

            override def doOnComplete(access: Access): Unit = {
              allTagsAreAffected(access)

              withClue("account data") {
                access.get(CacheKey.AccountData(aliceAddr, "x")) shouldBe RemoteData.Absence
              }

              withClue("transaction") {
                access.get(CacheKey.Transaction(transactionId)) shouldBe RemoteData.Unknown
              }

              withClue("asset") {
                access.get(CacheKey.Asset(asset)) shouldBe RemoteData.Absence
              }

              withClue("waves balance") {
                access.get(CacheKey.AccountBalance(aliceAddr, Asset.Waves)) shouldBe RemoteData.Cached(0L)
              }

              withClue("issued asset balance") {
                access.get(CacheKey.AccountBalance(bobAddr, asset)) shouldBe RemoteData.Cached(0L)
              }

              withClue("lease balance") {
                access.get(CacheKey.AccountLeaseBalance(aliceAddr)) shouldBe RemoteData.Cached(LeaseBalance(0L, 0L))
              }

              withClue("account script") {
                access.get(CacheKey.AccountScript(aliceAddr)) shouldBe RemoteData.Absence
              }
            }
          }.runTest()
        }
      }
    }

    "undo after" - {
      "append" - {
        "block" - {
          "ignores unrelated updates" in new Test {
            private val filledAppend = mkFilledAppendBlockEvent(2)

            override val preEvents           = Seq(mkEmptyAppendBlockEvent(1))
            override val trackAffectedEvents = Seq(filledAppend)

            override def doOnComplete(access: Access): Unit = {
              noTagsAreAffected(access.blockchain.undo(List(filledAppend)))
              allDataIsUnknownCheck(access)
            }
          }.runTest()

          "can get the restored data" in new Test {
            private val filledAppend = mkFilledAppendBlockEvent(2)

            override val preEvents           = Seq(mkEmptyAppendBlockEvent(1))
            override val trackAffectedEvents = Seq(filledAppend)

            override val dependencies = allDependencies

            override def doOnComplete(access: Access): Unit = {
              allTagsAreAffected(access.blockchain.undo(List(filledAppend)))
              allDataIsRestoredCheck(access)
            }
          }.runTest()
        }

        "micro block" - {
          "ignores unrelated updates" in new Test {
            private val block2     = mkEmptyAppendBlockEvent(2)
            private val microBlock = mkFilledAppendMicroBlockEvent(2)

            override val preEvents           = Seq(mkEmptyAppendBlockEvent(1))
            override val trackAffectedEvents = Seq(block2, microBlock)

            override def doOnComplete(access: Access): Unit = {
              noTagsAreAffected(access.blockchain.undo(List(microBlock, block2))) // Liquid block
              allDataIsUnknownCheck(access)
            }
          }.runTest()

          "can get the restored data" - {
            "one micro block" in new Test {
              private val block2 = mkFilledAppendBlockEvent(2)
              private val microBlock = mkMicroBlockAppendEvent(
                2,
                1,
                1,
                _.withStateUpdate(
                  StateUpdate.defaultInstance.withDataEntries(Seq(mkDataEntryUpdate(aliceAddr, "x", 1L.some, 2L.some)))
                )
              )

              override val preEvents           = Seq(mkEmptyAppendBlockEvent(1))
              override val trackAffectedEvents = Seq(block2, microBlock)

              override val dependencies = allDependencies

              override def doOnComplete(access: Access): Unit = {
                allTagsAreAffected(access.blockchain.undo(List(microBlock, block2))) // Liquid block
                allDataIsRestoredCheck(access)
              }
            }.runTest()
          }
        }
      }

      "rollback to" - {
        "block - does nothing" in new Test {
          private val emptyAppend = mkEmptyAppendBlockEvent(2)
          private val rollback    = mkRollbackEvent(emptyAppend)

          override val trackAffectedEvents = Seq(
            mkFilledAppendBlockEvent(1),
            emptyAppend,
            rollback
          )

          override val dependencies = allDependencies

          override def doOnComplete(access: Access): Unit = {
            noTagsAreAffected(access.blockchain.undo(List(rollback)))
            allDataIsCachedCheck(access)
            withClue("height") { access.blockchain.height shouldBe 1 }
          }
        }.runTest()

        "micro block - does nothing" in new Test {
          private val emptyAppend = mkEmptyAppendMicroBlockEvent(2)
          private val rollback    = mkRollbackEvent(emptyAppend)

          override val trackAffectedEvents = Seq(
            mkFilledAppendBlockEvent(1),
            mkEmptyAppendBlockEvent(2),
            emptyAppend,
            rollback
          )

          override val dependencies = allDependencies

          override def doOnComplete(access: Access): Unit = {
            noTagsAreAffected(access.blockchain.undo(List(rollback)))
            allDataIsCachedCheck(access)
            withClue("height") { access.blockchain.height shouldBe 2 }
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

  // TODO add aliases
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
          amountAfter = Amount(toByteString(Asset.Waves), 1L).some
        ),
        StateUpdate.BalanceUpdate(
          address = bobAddr.toByteString,
          amountAfter = Amount(toByteString(asset), 2L).some
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

  private def mkEmptyAppendMicroBlockEvent(height: Int) = mkMicroBlockAppendEvent(height, 1, 1)

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

  private val testBlockchainApi = new TestBlockchainApi()(monix.execution.schedulers.TestScheduler()) {
    override def getCurrentBlockchainHeight(): Height = Height(1)
    override def getActivatedFeatures(height: Height): Map[Short, Height] =
      DefaultBlockchainSettings.functionalitySettings.preActivatedFeatures.view.mapValues(Height(_)).toMap
  }

  type Tag = Int
  private abstract class Test {
    val allDependencies: Map[CacheKey, Tag] = Map(
      CacheKey.AccountData(aliceAddr, "x")                    -> 1,
      CacheKey.Transaction(transactionId)                     -> 2,
      CacheKey.Height                                         -> 3,
      CacheKey.Alias(Alias.create("foo-alias").explicitGet()) -> 4,
      CacheKey.Asset(asset)                                   -> 5,
      CacheKey.AccountBalance(aliceAddr, Asset.Waves)         -> 6,
      CacheKey.AccountBalance(bobAddr, asset)                 -> 7,
      CacheKey.AccountLeaseBalance(aliceAddr)                 -> 8,
      CacheKey.AccountScript(aliceAddr)                       -> 9
    )

    val preEvents: Seq[BlockchainUpdated]           = Seq.empty
    val trackAffectedEvents: Seq[BlockchainUpdated] = Seq.empty
    val dependencies: Map[CacheKey, Tag]            = Map.empty

    def doOnComplete(access: Access): Unit
    def runTest(): Unit = {
      withDb { db =>
        db.readWrite { implicit rw =>
          val blockchain = SharedBlockchainStorage[Tag](
            settings = SharedBlockchainStorage.Settings(
              blockchain = DefaultBlockchainSettings,
              commonCache = CommonCache.Settings(ConfigMemorySize.ofBytes(1 << 20))
            ),
            db = db,
            persistentCaches = DefaultPersistentCaches(db),
            blockchainApi = testBlockchainApi
          )

          dependencies.foreach(Function.tupled(blockchain.addDependent))
          preEvents.foreach(blockchain.process)
          val affectedTags = trackAffectedEvents.foldLeft(AffectedTags.empty[Int])((r, event) => r ++ blockchain.process(event))
          doOnComplete(new Access(blockchain, affectedTags))
        }
      }
    }

    // TODO move to access?
    def noTagsAreAffected(access: Access): Unit = noTagsAreAffected(access.affectedTags)

    def noTagsAreAffected(affectedTags: AffectedTags[Int]): Unit = withClue("affected tags") {
      affectedTags shouldBe empty
    }

    def allTagsAreAffected(access: Access): Unit = allTagsAreAffected(access.affectedTags)

    def allTagsAreAffected(affectedTags: AffectedTags[Int]): Unit = withClue("affected tags") {
      affectedTags shouldBe AffectedTags((1 to 9).toSet)
    }

    def allDataIsUnknownCheck(access: Access): Unit = {
      withClue("account data") {
        access.get(CacheKey.AccountData(aliceAddr, "x")) shouldBe RemoteData.Unknown
      }

      withClue("transaction") {
        access.get(CacheKey.Transaction(transactionId)) shouldBe RemoteData.Unknown
      }

      withClue("asset") {
        access.get(CacheKey.Asset(asset)) shouldBe RemoteData.Unknown
      }

      withClue("waves balance") {
        access.get(CacheKey.AccountBalance(aliceAddr, Asset.Waves)) shouldBe RemoteData.Unknown
      }

      withClue("issued asset balance") {
        access.get(CacheKey.AccountBalance(bobAddr, asset)) shouldBe RemoteData.Unknown
      }

      withClue("lease balance") {
        access.get(CacheKey.AccountLeaseBalance(aliceAddr)) shouldBe RemoteData.Unknown
      }

      withClue("account script") {
        access.get(CacheKey.AccountScript(aliceAddr)) shouldBe RemoteData.Unknown
      }
    }

    def allDataIsRestoredCheck(access: Access): Unit = {
      withClue("account data") {
        access.get(CacheKey.AccountData(aliceAddr, "x")) shouldBe RemoteData.Unknown
      }

      withClue("transaction") {
        access.get(CacheKey.Transaction(transactionId)) shouldBe RemoteData.Unknown
      }

      withClue("asset") {
        access.get(CacheKey.Asset(asset)) shouldBe RemoteData.Unknown
      }

      withClue("waves balance") {
        access.get(CacheKey.AccountBalance(aliceAddr, Asset.Waves)) shouldBe RemoteData.Cached(0L)
      }

      withClue("issued asset balance") {
        access.get(CacheKey.AccountBalance(bobAddr, asset)) shouldBe RemoteData.Cached(0L)
      }

      withClue("lease balance") {
        access.get(CacheKey.AccountLeaseBalance(aliceAddr)) shouldBe RemoteData.Cached(LeaseBalance(0L, 0L))
      }

      withClue("account script") {
        access.get(CacheKey.AccountScript(aliceAddr)) shouldBe RemoteData.Unknown
      }
    }

    def allDataIsCachedCheck(access: Access): Unit = {
      withClue("account data") {
        access.get(CacheKey.AccountData(aliceAddr, "x")) shouldBe RemoteData.Cached(IntegerDataEntry("x", 1L))
      }

      withClue("transaction") {
        access.get(CacheKey.Transaction(transactionId)) shouldBe RemoteData.Cached(Height(1))
      }

      withClue("asset") {
        access.get(CacheKey.Asset(asset)).map(_.assetDescription) shouldBe RemoteData.Cached(assetDescription)
      }

      withClue("waves balance") {
        access.get(CacheKey.AccountBalance(aliceAddr, Asset.Waves)) shouldBe RemoteData.Cached(1L)
      }

      withClue("issued asset balance") {
        access.get(CacheKey.AccountBalance(bobAddr, asset)) shouldBe RemoteData.Cached(2L)
      }

      withClue("lease balance") {
        access.get(CacheKey.AccountLeaseBalance(aliceAddr)) shouldBe RemoteData.Cached(LeaseBalance(4L, 3L))
      }

      withClue("account script") {
        access.get(CacheKey.AccountScript(aliceAddr)).map(_.scriptInfo) shouldBe RemoteData.Cached(accountScript)
      }
    }
  }

  private class Access(val blockchain: SharedBlockchainStorage[Tag], val affectedTags: AffectedTags[Tag])(implicit ctx: ReadWrite) {
    def get[T <: CacheKey](key: T): RemoteData[T#ValueT] = blockchain.getCached(key)
  }

  // TODO
  private def toByteString(asset: Asset): ByteString = asset.fold(ByteString.EMPTY)(_.id.toByteString)

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

}
