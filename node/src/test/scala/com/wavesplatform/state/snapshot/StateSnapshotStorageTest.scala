package com.wavesplatform.state.snapshot

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.TestValues.fee
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, Recipient}
import com.wavesplatform.protobuf.ByteStrExt
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.AssetStatic
import com.wavesplatform.state.*
import com.wavesplatform.state.TxMeta.Status.{Failed, Succeeded}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status.{Active, Cancelled}
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.{NumericExt, PropSpec}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{EthTxGenerator, Transaction, TxHelpers, TxNonNegativeAmount}

import scala.collection.immutable.VectorMap
import scala.math.pow

class StateSnapshotStorageTest extends PropSpec with WithDomain {
  property("transaction snapshot storage") {
    withDomain(RideV6.configure(_.copy(minAssetInfoUpdateInterval = 2))) { d =>
      val sender           = secondSigner
      val senderAddress    = secondAddress
      val recipientSigner  = TxHelpers.signer(2)
      val recipient        = recipientSigner.toAddress
      val recipientSigner2 = TxHelpers.signer(3)
      val recipient2       = recipientSigner2.toAddress

      def assertSnapshot(tx: Transaction, expected: StateSnapshot, failed: Boolean = false): Unit = {
        if (failed) d.appendAndAssertFailed(tx) else d.appendAndAssertSucceed(tx)
        d.appendBlock()
        val status = if (failed) Failed else Succeeded
        StateSnapshot.fromProtobuf(d.rocksDBWriter.transactionSnapshot(tx.id()).get) shouldBe (expected, status)
      }

      // Genesis
      assertSnapshot(
        genesis(senderAddress),
        StateSnapshot(
          balances = VectorMap((senderAddress, Waves) -> ENOUGH_AMT)
        )
      )

      // Payment
      assertSnapshot(
        payment(sender, recipient),
        StateSnapshot(
          balances = VectorMap(
            (recipient, Waves)     -> 1.waves,
            (senderAddress, Waves) -> (d.balance(senderAddress) - 1.waves - fee)
          )
        )
      )

      // Transfer
      assertSnapshot(
        transfer(sender, recipient),
        StateSnapshot(
          balances = VectorMap(
            (recipient, Waves)     -> 2.waves,
            (senderAddress, Waves) -> (d.balance(senderAddress) - 1.waves - fee)
          )
        )
      )

      // Issue
      val script  = TestCompiler(V6).compileExpression("true")
      val issueTx = issue(sender, script = Some(script))
      val asset   = IssuedAsset(issueTx.id())
      assertSnapshot(
        issueTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, asset) -> issueTx.quantity.value,
            (senderAddress, Waves) -> (d.balance(senderAddress) - 1.waves)
          ),
          assetStatics = VectorMap(
            asset -> AssetStatic(asset.id.toByteString, issueTx.id().toByteString, sender.publicKey.toByteString, issueTx.decimals.value)
          ),
          assetVolumes = Map(
            asset -> AssetVolumeInfo(isReissuable = true, BigInt(issueTx.quantity.value))
          ),
          assetNamesAndDescriptions = Map(
            asset -> AssetInfo(issueTx.name, issueTx.description, Height(d.solidStateHeight + 2))
          ),
          assetScripts = Map(
            asset -> AssetScriptInfo(script, 0)
          )
        )
      )

      // Reissue
      assertSnapshot(
        reissue(asset, sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, asset) -> (issueTx.quantity.value + 1000),
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          ),
          assetVolumes = Map(
            asset -> AssetVolumeInfo(isReissuable = true, BigInt(issueTx.quantity.value + 1000))
          )
        )
      )

      // Burn
      assertSnapshot(
        burn(asset, sender = sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, asset) -> (issueTx.quantity.value + 1000 - 1),
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          ),
          assetVolumes = Map(
            asset -> AssetVolumeInfo(isReissuable = true, BigInt(issueTx.quantity.value + 1000 - 1))
          )
        )
      )

      // Exchange
      d.appendBlock(
        transfer(to = recipient2, amount = 1.waves),
        transfer(from = sender, to = recipient2, amount = 1.waves, asset = asset, fee = fee)
      )
      val order1         = order(BUY, asset, Waves, matcher = sender, sender = recipientSigner, amount = 123, price = 40_000_000, fee = 777)
      val order2         = order(SELL, asset, Waves, matcher = sender, sender = recipientSigner2, amount = 123, price = 40_000_000, fee = 888)
      val priceAssetDiff = ((order1.amount.value * order1.price.value) / pow(10, 8)).toLong
      assertSnapshot(
        exchange(order1, order2, sender, amount = 123, price = 40_000_000, buyMatcherFee = 777, sellMatcherFee = 888),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee + order1.matcherFee.value + order2.matcherFee.value),
            (recipient, asset)     -> (d.balance(recipient, asset) + order1.amount.value),
            (recipient, Waves)     -> (d.balance(recipient) - order1.matcherFee.value - priceAssetDiff),
            (recipient2, asset)    -> (d.balance(recipient2, asset) - order1.amount.value),
            (recipient2, Waves)    -> (d.balance(recipient2) - order2.matcherFee.value + priceAssetDiff)
          ),
          orderFills = Map(
            order1.id() -> VolumeAndFee(order1.amount.value, order1.matcherFee.value),
            order2.id() -> VolumeAndFee(order2.amount.value, order2.matcherFee.value)
          )
        )
      )

      // Lease
      val leaseTx = lease(sender, recipient, fee = fee)
      assertSnapshot(
        leaseTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
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

      // Lease cancel
      val leaseCancelTx = leaseCancel(leaseTx.id(), sender, fee = fee)
      assertSnapshot(
        leaseCancelTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
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

      // Create alias
      assertSnapshot(
        createAlias("alias", sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          ),
          aliases = Map(Alias.create("alias").explicitGet() -> senderAddress)
        )
      )

      // Mass transfer
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
            (senderAddress, Waves)                 -> (d.balance(senderAddress) - fee - 123 - 456),
            (TxHelpers.signer(4).toAddress, Waves) -> 123,
            (TxHelpers.signer(5).toAddress, Waves) -> 456
          )
        )
      )

      // Data
      assertSnapshot(
        dataSingle(sender, "key", "value"),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          ),
          accountData = Map(
            senderAddress -> Map(
              "key" -> StringDataEntry("key", "value")
            )
          )
        )
      )

      // Set script
      assertSnapshot(
        setScript(sender, script),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          ),
          accountScripts = Map(
            sender.publicKey -> Some(AccountScriptInfo(sender.publicKey, script, 0))
          )
        )
      )

      // Sponsor
      val issue2     = issue(sender)
      val asset2     = IssuedAsset(issue2.id())
      val sponsorFee = 123456
      d.appendBlock(issue2)
      assertSnapshot(
        sponsor(asset2, Some(sponsorFee), sender),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - 1.waves)
          ),
          sponsorships = Map(
            asset2 -> SponsorshipValue(sponsorFee)
          )
        )
      )

      // Set asset script
      assertSnapshot(
        setAssetScript(sender, asset, script),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - 1.waves)
          ),
          assetScripts = Map(
            asset -> AssetScriptInfo(script, 0)
          )
        )
      )

      // Invoke
      val dApp = TestCompiler(V6).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   Issue("name", "description", 1000, 4, true, unit, 0),
           |   Lease(i.caller, 123),
           |   StringEntry("key", "abc")
           | ]
           |
           | @Callable(i)
           | func fail() = {
           |   strict c = ${(1 to 6).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
           |   if (true) then throw() else []
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(recipientSigner, dApp))
      val invokeTx = invoke(invoker = sender, dApp = recipient, fee = 1.005.waves)
      def invokeNonTransferSnapshot(senderAddress: Address, dAppPk: PublicKey, invokeId: ByteStr, height: Int = d.blockchain.height + 1) = {
        val dAppAssetId = IssuedAsset(Issue.calculateId(4, "description", true, "name", 1000, 0, invokeId))
        val leaseId     = Lease.calculateId(Lease(Recipient.Address(ByteStr(senderAddress.bytes)), 123, 0), invokeId)
        StateSnapshot(
          balances = VectorMap(
            (dAppPk.toAddress, dAppAssetId) -> 1000
          ),
          leaseBalances = Map(
            dAppPk.toAddress -> LeaseBalance(0, 123),
            senderAddress    -> LeaseBalance(123, 0)
          ),
          assetStatics = VectorMap(
            dAppAssetId -> AssetStatic(dAppAssetId.id.toByteString, invokeId.toByteString, dAppPk.toByteString, 4)
          ),
          assetVolumes = Map(
            dAppAssetId -> AssetVolumeInfo(true, 1000)
          ),
          assetNamesAndDescriptions = Map(
            dAppAssetId -> AssetInfo("name", "description", Height(height))
          ),
          leaseStates = Map(
            leaseId -> LeaseDetails(dAppPk, senderAddress, 123, Active, invokeId, height)
          ),
          accountData = Map(
            dAppPk.toAddress -> Map("key" -> StringDataEntry("key", "abc"))
          )
        )
      }
      assertSnapshot(
        invokeTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - 1.005.waves)
          )
        ) |+| invokeNonTransferSnapshot(senderAddress, recipientSigner.publicKey, invokeTx.id())
      )

      // Update asset info
      val updateAssetTx = updateAssetInfo(asset2.id, sender = sender)
      assertSnapshot(
        updateAssetTx,
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          ),
          assetNamesAndDescriptions = Map(
            asset2 -> AssetInfo(updateAssetTx.name, updateAssetTx.description, Height(d.solidStateHeight + 2))
          )
        )
      )

      // Ethereum transfer
      val ethTransfer = EthTxGenerator.generateEthTransfer(defaultEthSigner, recipient, 1, asset2)
      d.appendBlock(transfer(sender, ethTransfer.senderAddress()), transfer(sender, ethTransfer.senderAddress(), asset = asset2))
      assertSnapshot(
        ethTransfer,
        StateSnapshot(
          balances = VectorMap(
            (ethTransfer.senderAddress(), Waves)  -> (d.balance(ethTransfer.senderAddress()) - 100_000),
            (ethTransfer.senderAddress(), asset2) -> (d.balance(ethTransfer.senderAddress(), asset2) - 1),
            (recipient, asset2)                   -> 1
          )
        )
      )

      // Ethereum invoke
      val ethInvoke = EthTxGenerator.generateEthInvoke(defaultEthSigner, address = recipient2, "default", Seq(), Seq(), 1.005.waves)
      d.appendBlock(transfer(sender, ethInvoke.senderAddress()))
      d.appendBlock(setScript(recipientSigner2, dApp))
      assertSnapshot(
        ethInvoke,
        StateSnapshot(
          balances = VectorMap(
            (ethInvoke.senderAddress(), Waves) -> (d.balance(ethInvoke.senderAddress()) - 1.005.waves)
          )
        ) |+| invokeNonTransferSnapshot(ethInvoke.senderAddress(), recipientSigner2.publicKey, ethInvoke.id())
      )

      // Failed invoke
      assertSnapshot(
        invoke(invoker = sender, func = Some("fail"), dApp = recipient, fee = fee),
        StateSnapshot(
          balances = VectorMap(
            (senderAddress, Waves) -> (d.balance(senderAddress) - fee)
          )
        ),
        failed = true
      )
    }
  }
}
