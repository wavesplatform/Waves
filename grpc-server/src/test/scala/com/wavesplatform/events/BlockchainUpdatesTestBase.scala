package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdateResponse, GetBlockUpdatesRangeRequest}
import com.wavesplatform.events.fixtures.WavesTxChecks.{
  checkAssetUpdatesStateUpdates,
  checkAssetsScriptStateUpdates,
  checkAssetsStateUpdates,
  checkBalances,
  checkBurn,
  checkCreateAlias,
  checkDataEntriesStateUpdate,
  checkDataTransaction,
  checkEthereumTransaction,
  checkExchange,
  checkIndividualLeases,
  checkIssue,
  checkLease,
  checkLeaseCancel,
  checkLeasingForAddress,
  checkMassTransfer,
  checkMassTransferBalances,
  checkReissue,
  checkSetAssetScriptTransaction,
  checkSetScriptStateUpdate,
  checkSetScriptTransaction,
  checkSponsorFeeTransaction,
  checkTransfer,
  checkUpdateAssetInfoTransaction
}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.Order.Version
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.assets.{
  BurnTransaction,
  IssueTransaction,
  ReissueTransaction,
  SetAssetScriptTransaction,
  SponsorFeeTransaction,
  UpdateAssetInfoTransaction
}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.utils.EthConverters.EthereumKeyPairExt
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, EthereumTransaction, TxHelpers}
import org.scalatest.concurrent.ScalaFutures
import org.web3j.crypto.Bip32ECKeyPair

abstract class BlockchainUpdatesTestBase extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings             = DomainPresets.RideV6
  val amount: Long                               = 9000000
  val additionalAmount: Long                     = 5000000
  val customFee: Long                            = 5234567L
  val customAssetIssueFee                        = 234567654L
  val firstTxParticipant: SeedKeyPair            = TxHelpers.signer(2)
  val firstTxParticipantAddress: Address         = firstTxParticipant.toAddress
  val ethKeyPairExt: EthereumKeyPairExt          = EthereumKeyPairExt(firstTxParticipant)
  val firstTxParticipantEthereum: Bip32ECKeyPair = ethKeyPairExt.toEthKeyPair
  val firstTxParticipantEthereumAddress: Address = ethKeyPairExt.toEthWavesAddress
  val ethKeyPair: SeedKeyPair                    = SeedKeyPair.apply(firstTxParticipantEthereum.getPrivateKeyBytes33)
  val firstTxParticipantBalanceBefore: Long      = 20.waves
  val secondTxParticipant: SeedKeyPair           = TxHelpers.signer(3)
  val secondTxParticipantAddress: Address        = secondTxParticipant.toAddress
  val secondTxParticipantPKHash: Array[Byte]     = secondTxParticipantAddress.publicKeyHash
  val secondTxParticipantBalanceBefore: Long     = 20.waves
  val recipients: Seq[ParsedTransfer]            = TxHelpers.accountSeqGenerator(100, additionalAmount)
  val firstToken: IssueTransaction               = TxHelpers.issue(firstTxParticipant, amount = 2000000000L, 2)
  val firstTokenAsset: IssuedAsset               = firstToken.asset
  val firstTokenQuantity: Long                   = firstToken.quantity.value
  val firstTokenFee: Long                        = firstToken.fee.value
  val secondToken: IssueTransaction              = TxHelpers.issue(secondTxParticipant, amount = 6000000000L, 6)
  val secondTokenAsset: IssuedAsset              = secondToken.asset
  val secondTokenQuantity: Long                  = secondToken.quantity.value
  val integerDataEntry: IntegerDataEntry         = IntegerDataEntry.apply("Integer", 3550000L)
  val booleanDataEntry: BooleanDataEntry         = BooleanDataEntry.apply("Boolean", value = true)
  val stringDataEntry: StringDataEntry           = StringDataEntry.apply("String", "test")
  val binaryDataEntry: BinaryDataEntry           = BinaryDataEntry.apply("Binary", ByteStr.apply(firstTxParticipantAddress.bytes))
  val entries: Seq[DataEntry[?]]                 = Seq(booleanDataEntry, integerDataEntry, stringDataEntry, binaryDataEntry)
  val defaultScript: Option[Script]              = Option(TxHelpers.script("true"))
  val complexScriptBefore: Option[Script]        = Option.apply(TxHelpers.script("true".stripMargin))
  val complexScriptAfter: Script                 = TxHelpers.script("false".stripMargin)
  val testScript: Script = TxHelpers.script(s"""{-# STDLIB_VERSION 6 #-}
                                               |{-# CONTENT_TYPE DAPP #-}
                                               |{-# SCRIPT_TYPE ACCOUNT #-}
                                               |
                                               |@Verifier(tx)
                                               |func verify () = match(tx) {
                                               |    case _ =>
                                               |      if (
                                               |        ${(1 to 9).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || \n")}
                                               |      ) then true else true
                                               |}""".stripMargin)

  protected def createOrders(orderType: OrderType, orderSender: SeedKeyPair, orderVersion: Version): Order = {
    TxHelpers.order(
      orderType,
      secondToken.asset,
      firstToken.asset,
      Waves,
      amount = 50000L,
      price = 400000000L,
      fee = customFee,
      sender = orderSender,
      matcher = firstTxParticipant,
      version = orderVersion
    )
  }

  protected def checkingAlias(append: Append, aliasTx: CreateAliasTransaction): Unit = {
    val firstTxParticipantBalanceAfter: Long = firstTxParticipantBalanceBefore - aliasTx.fee.value
    checkCreateAlias(append.transactionIds.head, append.transactionAt(0), aliasTx)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map((firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter))
    )
  }

  protected def checkingTransferTx(append: Append, transferTx: TransferTransaction): Unit = {
    val firstTxParticipantBalanceAfter  = firstTxParticipantBalanceBefore - customFee - amount
    val secondTxParticipantBalanceAfter = secondTxParticipantBalanceBefore + amount
    checkTransfer(append.transactionIds.head, append.transactionAt(0), transferTx, secondTxParticipantAddress.publicKeyHash)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves)  -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter),
        (secondTxParticipantAddress, Waves) -> (secondTxParticipantBalanceBefore, secondTxParticipantBalanceAfter)
      )
    )
  }

  protected def checkingIssueTx(append: Append, issue: IssueTransaction, isNft: Boolean): Unit = {
    val assetDetails                   = append.transactionStateUpdates.head.assets.head
    val issueQuantity                  = issue.quantity.value
    val firstTxParticipantBalanceAfter = firstTxParticipantBalanceBefore - issue.fee.value

    checkIssue(append.transactionIds.head, append.transactionAt(0), issue)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves)       -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter),
        (firstTxParticipantAddress, issue.asset) -> (0, issueQuantity)
      )
    )
    checkAssetsStateUpdates(assetDetails.after, issue, isNft, issue.quantity.value)
    if (issue.script.isDefined) {
      checkAssetsScriptStateUpdates(assetDetails.after.get.scriptInfo, issue.script.get.bytes.value().arr)
    }
  }

  protected def checkingReissueTx(append: Append, reissue: ReissueTransaction, issue: IssueTransaction): Unit = {
    val assetDetails                           = append.transactionStateUpdates.head.assets.head
    val quantityAfter                          = amount + additionalAmount
    val firstTxParticipantBalanceBeforeReissue = firstTxParticipantBalanceBefore - issue.fee.value
    val firstTxParticipantBalanceAfterReissue  = firstTxParticipantBalanceBeforeReissue - reissue.fee.value

    checkReissue(append.transactionIds.head, append.transactionAt(0), reissue)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves)         -> (firstTxParticipantBalanceBeforeReissue, firstTxParticipantBalanceAfterReissue),
        (firstTxParticipantAddress, reissue.asset) -> (amount, quantityAfter)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
    checkAssetsStateUpdates(assetDetails.after, reissue, isNft = false, quantityAfter)
  }

  protected def checkingBurnTx(append: Append, burn: BurnTransaction, issue: IssueTransaction): Unit = {
    val assetDetails                        = append.transactionStateUpdates.head.assets.head
    val amountAfterTx                       = amount - additionalAmount
    val firstTxParticipantBalanceBeforeBurn = firstTxParticipantBalanceBefore - issue.fee.value
    val firstTxParticipantBalanceAfterBurn  = firstTxParticipantBalanceBeforeBurn - burn.fee.value

    checkBurn(append.transactionIds.head, append.transactionAt(0), burn)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves)      -> (firstTxParticipantBalanceBeforeBurn, firstTxParticipantBalanceAfterBurn),
        (firstTxParticipantAddress, burn.asset) -> (amount, amountAfterTx)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
    checkAssetsStateUpdates(assetDetails.after, burn, isNft = false, amountAfterTx)
  }

  protected def checkingExchangeTx(append: Append, exchangeTx: ExchangeTransaction, normalizedPrice: Long, orderAmount: Long): Unit = {
    val amountAssetQuantity                      = secondToken.quantity.value
    val firstTxParticipantBalanceBeforeExchange  = firstTxParticipantBalanceBefore - firstTokenFee
    val firstTxParticipantBalanceAfterExchange   = firstTxParticipantBalanceBeforeExchange - exchangeTx.fee.value + customFee
    val secondTxParticipantBalanceBeforeExchange = secondTxParticipantBalanceBefore - secondToken.fee.value
    val secondTxParticipantBalanceAfterExchange  = secondTxParticipantBalanceBeforeExchange - customFee

    checkExchange(append.transactionIds.head, append.transactionAt(0), exchangeTx)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves)              -> (firstTxParticipantBalanceBeforeExchange, firstTxParticipantBalanceAfterExchange),
        (secondTxParticipantAddress, firstToken.asset)  -> (0, normalizedPrice),
        (firstTxParticipantAddress, secondToken.asset)  -> (0, orderAmount),
        (secondTxParticipantAddress, Waves)             -> (secondTxParticipantBalanceBeforeExchange, secondTxParticipantBalanceAfterExchange),
        (firstTxParticipantAddress, firstToken.asset)   -> (firstTokenQuantity, firstTokenQuantity - normalizedPrice),
        (secondTxParticipantAddress, secondToken.asset) -> (amountAssetQuantity, amountAssetQuantity - orderAmount)
      )
    )
  }

  protected def checkingLeaseTx(append: Append, lease: LeaseTransaction): Unit = {
    val leaseId = lease.id.value().arr
    checkLease(append.transactionIds.head, append.transactionAt(0), lease, secondTxParticipantPKHash)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceBefore - customFee)
      )
    )
    checkLeasingForAddress(
      append.transactionStateUpdates.head.leasingForAddress,
      Map(
        (firstTxParticipantAddress, 0L, amount)  -> (0L, 0L),
        (secondTxParticipantAddress, amount, 0L) -> (0L, 0L)
      )
    )
    checkIndividualLeases(
      append.transactionStateUpdates.head.individualLeases,
      Map(
        (LeaseStatus.Active, amount) -> (leaseId, lease.sender.arr, lease.recipient.bytes, leaseId)
      )
    )
  }

  protected def checkingLeaseCancelTx(append: Append, leaseCancel: LeaseCancelTransaction, lease: LeaseTransaction): Unit = {
    val leaseId                           = leaseCancel.leaseId.arr
    val firstTxParticipantBalanceBeforeTx = firstTxParticipantBalanceBefore - lease.fee.value
    val firstTxParticipantBalanceAfterTx  = firstTxParticipantBalanceBeforeTx - leaseCancel.fee.value
    checkLeaseCancel(append.transactionIds.head, append.transactionAt(0), leaseCancel)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map(
        (firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBeforeTx, firstTxParticipantBalanceAfterTx)
      )
    )
    checkLeasingForAddress(
      append.transactionStateUpdates.head.leasingForAddress,
      Map(
        (firstTxParticipantAddress, 0L, 0L)  -> (0L, amount),
        (secondTxParticipantAddress, 0L, 0L) -> (amount, 0L)
      )
    )
    checkIndividualLeases(
      append.transactionStateUpdates.head.individualLeases,
      Map(
        (LeaseStatus.Inactive, amount) -> (leaseId, lease.sender.arr, lease.recipient.bytes, leaseId)
      )
    )
  }

  protected def checkingMassTransfer(append: Append, massTransfer: MassTransferTransaction): Unit = {
    val firstTxParticipantBalanceBeforeTx     = firstTxParticipantBalanceBefore - firstTokenFee
    val firstTxParticipantBalanceAfterTx      = firstTxParticipantBalanceBeforeTx - massTransfer.fee.value
    val firstTxParticipantAssetBalanceAfterTx = firstTokenQuantity - additionalAmount * recipients.size

    val balancesMap = Map(
      (firstTxParticipantAddress, Waves)                -> (firstTxParticipantBalanceBeforeTx, firstTxParticipantBalanceAfterTx),
      (firstTxParticipantAddress, massTransfer.assetId) -> (firstTokenQuantity, firstTxParticipantAssetBalanceAfterTx)
    ) ++ recipients.map(r => (Address.fromBytes(r.address.bytes).explicitGet(), massTransfer.assetId) -> (0L, additionalAmount)).toMap
    checkMassTransfer(
      append.transactionIds.head,
      append.transactionAt(0),
      massTransfer,
      recipients.map(r =>
        r.address match {
          case address: Address => Some(address.publicKeyHash).get
          case _                => fail("not an address")
        }
      )
    )
    checkMassTransferBalances(append.transactionStateUpdates.head.balances, balancesMap)
  }

  protected def checkingDataTransfer(append: Append, data: DataTransaction): Unit = {
    val txUpdates = append.transactionStateUpdates.head
    checkDataTransaction(append.transactionIds.head, append.transactionAt(0), data)
    checkBalances(
      txUpdates.balances,
      Map((firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceBefore - customFee))
    )
    checkDataEntriesStateUpdate(txUpdates.dataEntries, data.data, firstTxParticipantAddress.bytes)
  }

  protected def checkingSetScript(append: Append, setScript: SetScriptTransaction): Unit = {
    val txUpdates = append.transactionStateUpdates.head
    checkSetScriptTransaction(append.transactionIds.head, append.transactionAt(0), setScript)
    checkBalances(
      txUpdates.balances,
      Map((firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceBefore - customFee))
    )
    checkSetScriptStateUpdate(txUpdates.scripts.head, setScript)
  }

  protected def checkingSetAssetScript(append: Append, setAssetScript: SetAssetScriptTransaction, issue: IssueTransaction): Unit = {
    val quantity                                        = issue.quantity.value
    val firstTxParticipantBalanceBeforeSetAssetScriptTx = firstTxParticipantBalanceBefore - issue.fee.value
    val firstTxParticipantBalanceAfterSetAssetScriptTx  = firstTxParticipantBalanceBeforeSetAssetScriptTx - setAssetScript.fee.value
    val txUpdates                                       = append.transactionStateUpdates.head
    val assetDetails                                    = txUpdates.assets.head
    checkSetAssetScriptTransaction(append.transactionIds.head, append.transactionAt(0), setAssetScript)
    checkBalances(
      txUpdates.balances,
      Map(
        (firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBeforeSetAssetScriptTx, firstTxParticipantBalanceAfterSetAssetScriptTx)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, quantity)
    checkAssetsScriptStateUpdates(assetDetails.before.get.scriptInfo, complexScriptBefore.get.bytes.value().arr)
    checkAssetsStateUpdates(assetDetails.after, issue, isNft = false, quantity)
    checkAssetsScriptStateUpdates(assetDetails.after.get.scriptInfo, complexScriptAfter.bytes.value().arr)
  }

  protected def checkingUpdateAssetInfo(append: Append, updateAssetInfo: UpdateAssetInfoTransaction): Unit = {
    val firstTxParticipantBalanceBeforeUpdateAssetInfoTx = firstTxParticipantBalanceBefore - firstTokenFee
    val firstTxParticipantBalanceAfterUpdateAssetInfoTx  = firstTxParticipantBalanceBeforeUpdateAssetInfoTx - updateAssetInfo.fee
    val txUpdates                                        = append.transactionStateUpdates.head
    val assetDetails                                     = txUpdates.assets.head
    checkUpdateAssetInfoTransaction(append.transactionIds.head, append.transactionAt(0), updateAssetInfo)
    checkBalances(
      txUpdates.balances,
      Map(
        (firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBeforeUpdateAssetInfoTx, firstTxParticipantBalanceAfterUpdateAssetInfoTx)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, firstToken, isNft = false, firstTokenQuantity)
    checkAssetUpdatesStateUpdates(assetDetails.after, updateAssetInfo)
  }

  protected def checkingSponsorFee(append: Append, sponsorFee: SponsorFeeTransaction, balanceBefore: Long, balanceAfter: Long): Unit = {
    val txUpdates    = append.transactionStateUpdates.head
    val assetDetails = txUpdates.assets.head

    checkSponsorFeeTransaction(append.transactionIds.head, append.transactionAt(0), sponsorFee)
    checkBalances(
      txUpdates.balances,
      Map((firstTxParticipantAddress, Waves) -> (balanceBefore, balanceAfter))
    )
    checkAssetsStateUpdates(assetDetails.before, firstToken, isNft = false, firstTokenQuantity)
    checkAssetsStateUpdates(assetDetails.after, firstToken, isNft = false, firstTokenQuantity)
  }

  protected def checkingEthereumTransfer(append: Append, ethereumTransfer: EthereumTransaction, ethAddress: Address): Unit = {
    val firstTxParticipantBalanceAfter = firstTxParticipantBalanceBefore - ethereumTransfer.fee
    val recipientTokenBalanceAfter     = secondTokenQuantity - amount

    val balances = append.transactionStateUpdates.head.balances
    checkEthereumTransaction(append.transactionIds.head, append.transactionAt(0), ethereumTransfer)
    checkBalances(
      balances,
      Map(
        (ethAddress, Waves)                            -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter),
        (ethAddress, secondTokenAsset)                 -> (secondTokenQuantity, recipientTokenBalanceAfter),
        (secondTxParticipantAddress, secondTokenAsset) -> (0, amount)
      )
    )
  }

  protected def addedBlocksAndSubscribe(exchangeTx: ExchangeTransaction)(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
    withGenerateSubscription(
      settings = currentSettings,
      balances = Seq(
        AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
        AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
      )
    ) { d =>
      d.appendBlock(firstToken)
      d.appendBlock(secondToken)
      d.appendMicroBlock(exchangeTx)
    }(f)
  }

  protected def addedBlocksAndGetBlockUpdate(exchangeTx: ExchangeTransaction, height: Int)(f: GetBlockUpdateResponse => Unit): Unit = {
    withGenerateGetBlockUpdate(
      height,
      settings = currentSettings,
      balances = Seq(
        AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
        AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
      )
    ) { d =>
      d.appendBlock(firstToken)
      d.appendBlock(secondToken)
      d.appendBlock(exchangeTx)
    }(f)
  }

  protected def addedBlocksAndGetBlockUpdateRange(exchangeTx: ExchangeTransaction, height: GetBlockUpdatesRangeRequest)(
      f: Seq[PBBlockchainUpdated] => Unit
  ): Unit = {
    withGenerateGetBlockUpdateRange(
      height,
      settings = currentSettings,
      balances = Seq(
        AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
        AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
      )
    ) { d =>
      d.appendBlock(firstToken)
      d.appendBlock(secondToken)
      d.appendBlock(exchangeTx)
      d.appendBlock()
    }(f)
  }
}
