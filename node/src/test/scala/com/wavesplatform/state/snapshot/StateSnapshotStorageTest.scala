package com.wavesplatform.state.snapshot

import com.wavesplatform.TestValues.fee
import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.ByteStrExt
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.AssetStatic
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status.{Active, Cancelled}
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.{NumericExt, PropSpec}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxNonNegativeAmount}

import scala.collection.immutable.VectorMap

class StateSnapshotStorageTest extends PropSpec with WithDomain {
  property("transaction snapshot storage") {
    withDomain(RideV6.configure(_.copy(minAssetInfoUpdateInterval = 2))) { d =>
      val sender          = secondSigner
      val senderAddress   = secondAddress
      val recipientSigner = TxHelpers.signer(2)
      val recipient       = recipientSigner.toAddress

      def assertSnapshot(tx: Transaction, expected: StateSnapshot): Unit = {
        d.appendAndAssertSucceed(tx)
        d.appendBlock()
        StateSnapshot.fromProtobuf(d.rocksDBWriter.transactionSnapshot(tx.id()).get)._1 shouldBe expected
      }

      assertSnapshot(
        genesis(senderAddress),
        StateSnapshot(
          balances = VectorMap((senderAddress, Waves) -> ENOUGH_AMT)
        )
      )
      assertSnapshot(
        payment(sender, recipient),
        StateSnapshot(
          balances = VectorMap(
            (recipient, Waves)     -> 1.waves,
            (senderAddress, Waves) -> (ENOUGH_AMT - 1.waves - fee)
          )
        )
      )
      assertSnapshot(
        transfer(sender, recipient),
        StateSnapshot(
          balances = VectorMap(
            (recipient, Waves)     -> 2.waves,
            (senderAddress, Waves) -> (ENOUGH_AMT - 2.waves - 2 * fee)
          )
        )
      )
      val script  = TestCompiler(V6).compileExpression("true")
      val issueTx = issue(sender, script = Some(script))
      val asset   = IssuedAsset(issueTx.id())
      assertSnapshot(
        issueTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, asset) -> issueTx.quantity.value,
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 2 * fee)
          ),
          assetStatics = Map(
            asset -> AssetStatic(asset.id.toByteString, issueTx.id().toByteString, sender.publicKey.toByteString, issueTx.decimals.value)
          ),
          assetVolumes = Map(
            asset -> AssetVolumeInfo(isReissuable = true, BigInt(issueTx.quantity.value))
          ),
          assetNamesAndDescriptions = Map(
            asset -> AssetInfo(issueTx.name, issueTx.description, Height(d.solidStateHeight + 2))
          ),
          assetScripts = Map(
            asset -> Some(AssetScriptInfo(script, 0))
          )
        )
      )
      assertSnapshot(
        reissue(asset, sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, asset) -> (issueTx.quantity.value + 1000),
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 3 * fee)
          ),
          assetVolumes = Map(
            asset -> AssetVolumeInfo(isReissuable = true, BigInt(issueTx.quantity.value + 1000))
          )
        )
      )
      assertSnapshot(
        burn(asset, sender = sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, asset) -> (issueTx.quantity.value + 1000 - 1),
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 4 * fee)
          ),
          assetVolumes = Map(
            asset -> AssetVolumeInfo(isReissuable = true, BigInt(issueTx.quantity.value + 1000 - 1))
          )
        )
      )
      val order1 = order(BUY, asset, Waves, matcher = sender, sender = recipientSigner, amount = 123, price = 456, fee = 777)
      val order2 = order(SELL, asset, Waves, matcher = sender, sender = recipientSigner, amount = 123, price = 456, fee = 888)
      assertSnapshot(
        exchange(order1, order2, sender, amount = 123, price = 456, buyMatcherFee = 777, sellMatcherFee = 888),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 5 * fee + order1.matcherFee.value + order2.matcherFee.value),
            (recipient, Waves)     -> (d.balance(recipient) - order1.matcherFee.value - order2.matcherFee.value)
          ),
          orderFills = Map(
            order1.id() -> VolumeAndFee(order1.amount.value, order1.matcherFee.value),
            order2.id() -> VolumeAndFee(order2.amount.value, order2.matcherFee.value)
          )
        )
      )
      val leaseTx = lease(sender, recipient, fee = fee)
      assertSnapshot(
        leaseTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 6 * fee + order1.matcherFee.value + order2.matcherFee.value)
          ),
          leaseBalances = Map(
            senderAddress -> LeaseBalance(0, leaseTx.amount.value),
            recipient     -> LeaseBalance(leaseTx.amount.value, 0)
          ),
          leaseStates = Map(
            leaseTx.id() -> LeaseDetails(sender.publicKey, recipient, leaseTx.amount.value, Active, leaseTx.id(), d.solidStateHeight + 2)
          )
        )
      )
      val leaseCancelTx = leaseCancel(leaseTx.id(), sender, fee = fee)
      assertSnapshot(
        leaseCancelTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 7 * fee + order1.matcherFee.value + order2.matcherFee.value)
          ),
          leaseBalances = Map(
            senderAddress -> LeaseBalance(0, 0),
            recipient     -> LeaseBalance(0, 0)
          ),
          leaseStates = Map(
            leaseTx.id() -> LeaseDetails(
              sender.publicKey,
              recipient,
              leaseTx.amount.value,
              Cancelled(d.solidStateHeight + 2, Some(leaseCancelTx.id())),
              leaseTx.id(),
              d.solidStateHeight
            )
          )
        )
      )
      assertSnapshot(
        createAlias("alias", sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 8 * fee + order1.matcherFee.value + order2.matcherFee.value)
          ),
          aliases = Map(Alias.create("alias").explicitGet() -> senderAddress)
        )
      )
      assertSnapshot(
        massTransfer(
          sender,
          fee = fee,
          to = Seq(
            ParsedTransfer(TxHelpers.signer(4).toAddress, TxNonNegativeAmount(123)),
            ParsedTransfer(TxHelpers.signer(5).toAddress, TxNonNegativeAmount(456))
          )
        ),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 9 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value),
            (TxHelpers.signer(4).toAddress, Waves) -> 123,
            (TxHelpers.signer(5).toAddress, Waves) -> 456
          )
        )
      )
      assertSnapshot(
        dataSingle(sender, "key", "value"),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 10 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value)
          ),
          accountData = Map(
            senderAddress -> Map(
              "key" -> StringDataEntry("key", "value")
            )
          )
        )
      )
      assertSnapshot(
        setScript(sender, script),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 3.waves - 11 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value)
          ),
          accountScripts = Map(
            sender.publicKey -> Some(AccountScriptInfo(sender.publicKey, script, 0))
          )
        )
      )
      val issue2     = issue(sender)
      val asset2     = IssuedAsset(issue2.id())
      val sponsorFee = 123456
      d.appendBlock(issue2)
      assertSnapshot(
        sponsor(asset2, Some(sponsorFee), sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 5.waves - 11 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value)
          ),
          sponsorships = Map(
            asset2 -> SponsorshipValue(sponsorFee)
          )
        )
      )
      assertSnapshot(
        setAssetScript(sender, asset, script),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 6.waves - 11 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value)
          ),
          assetScripts = Map(
            asset -> Some(AssetScriptInfo(script, 0))
          )
        )
      )
      val setDApp = setScript(
        recipientSigner,
        TestCompiler(V6).compileContract(
          """
            | @Callable(i)
            | func default() = []
          """.stripMargin
        )
      )
      d.appendBlock(setDApp)
      assertSnapshot(
        invoke(invoker = sender, dApp = recipient, fee = fee),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 6.waves - 12 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value)
          )
        )
      )
      val updateAssetTx = updateAssetInfo(asset2.id, sender = sender)
      assertSnapshot(
        updateAssetTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (ENOUGH_AMT - 6.waves - 13 * fee - 123 - 456 + order1.matcherFee.value + order2.matcherFee.value)
          ),
          assetNamesAndDescriptions = Map(
            asset2 -> AssetInfo(updateAssetTx.name, updateAssetTx.description, Height(d.solidStateHeight + 2))
          )
        )
      )
      val ethTransfer = EthTxGenerator.generateEthTransfer(defaultEthSigner, recipient, 1, asset2)
      d.appendBlock(transfer(sender, ethTransfer.senderAddress()), transfer(sender, ethTransfer.senderAddress(), asset = asset2))
      assertSnapshot(
        ethTransfer,
        StateSnapshot(
          balances = VectorMap(
            (ethTransfer.senderAddress(), Waves)  -> (1.waves - 100_000),
            (ethTransfer.senderAddress(), asset2) -> (1.waves - 1),
            (recipient, asset2)                   -> 1
          )
        )
      )
    }
  }
}
