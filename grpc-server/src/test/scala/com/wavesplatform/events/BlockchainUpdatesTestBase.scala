package com.wavesplatform.events

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeRequest}
import com.wavesplatform.events.fixtures.BlockchainUpdateGrpcMethod
import com.wavesplatform.events.fixtures.BlockchainUpdateGrpcMethod.{GetBlockUpdate, GetBlockUpdateRange, Subscribe}
import com.wavesplatform.events.fixtures.InvokeWavesTxCheckers.checkSimpleInvoke
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.events.protobuf.TransactionMetadata.InvokeScriptMetadata
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.*
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.secondSigner
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.assets.exchange.Order.Version
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.utils.EthConverters.EthereumKeyPairExt
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, DataTransaction, EthereumTransaction, Transaction, TxHelpers}
import org.scalactic.source.Position
import org.scalatest.Assertions
import org.scalatest.concurrent.ScalaFutures
import org.web3j.crypto.Bip32ECKeyPair

class BlockchainUpdatesTestBase extends FreeSpec with WithBUDomain with ScalaFutures {
  import BlockchainUpdatesTestBase.*
  val currentSettings: WavesSettings                   = DomainPresets.RideV6
  val amount: Long                                     = 9000000
  val additionalAmount: Long                           = 5000000
  val customFee: Long                                  = 5234567L
  val customAssetIssueFee                              = 234567654L
  val invokeFee: Long                                  = 1.005.waves
  val firstTxParticipant: SeedKeyPair                  = TxHelpers.signer(2)
  val firstTxParticipantAddress: Address               = firstTxParticipant.toAddress
  val ethKeyPairExt: EthereumKeyPairExt                = EthereumKeyPairExt(firstTxParticipant)
  val firstTxParticipantEthereum: Bip32ECKeyPair       = ethKeyPairExt.toEthKeyPair
  val firstTxParticipantEthereumAddress: Address       = ethKeyPairExt.toEthWavesAddress
  val firstTxParticipantEthereumPublicKey: Array[Byte] = firstTxParticipantEthereum.getPublicKey.toByteArray
  val ethKeyPair: SeedKeyPair                          = SeedKeyPair.apply(firstTxParticipantEthereum.getPrivateKeyBytes33)
  val firstTxParticipantBalanceBefore: Long            = 20.waves
  val secondTxParticipant: SeedKeyPair                 = TxHelpers.signer(3)
  val secondTxParticipantAddress: Address              = secondTxParticipant.toAddress
  val secondTxParticipantPKHash: Array[Byte]           = secondTxParticipantAddress.publicKeyHash
  val secondTxParticipantBalanceBefore: Long           = 20.waves
  val recipients: Seq[ParsedTransfer]                  = TxHelpers.accountSeqGenerator(100, additionalAmount)
  val firstToken: IssueTransaction                     = TxHelpers.issue(firstTxParticipant, amount = 2000000000L, 2)
  val firstTokenAsset: IssuedAsset                     = firstToken.asset
  val firstTokenQuantity: Long                         = firstToken.quantity.value
  val firstTokenFee: Long                              = firstToken.fee.value
  val secondToken: IssueTransaction                    = TxHelpers.issue(secondTxParticipant, amount = 6000000000L, 6)
  val secondTokenAsset: IssuedAsset                    = secondToken.asset
  val secondTokenQuantity: Long                        = secondToken.quantity.value
  val integerDataEntry: IntegerDataEntry               = IntegerDataEntry.apply("Integer", 3550000L)
  val booleanDataEntry: BooleanDataEntry               = BooleanDataEntry.apply("Boolean", value = true)
  val stringDataEntry: StringDataEntry                 = StringDataEntry.apply("String", "test")
  val binaryDataEntry: BinaryDataEntry                 = BinaryDataEntry.apply("Binary", ByteStr.apply(firstTxParticipantAddress.bytes))
  val expectDataEntries: Seq[DataEntry[?]]             = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferUnitNum * 2))
  val entries: Seq[DataEntry[?]]                       = Seq(booleanDataEntry, integerDataEntry, stringDataEntry, binaryDataEntry)
  val defaultScript: Option[Script]                    = Option(TxHelpers.script("true"))
  val complexScriptBefore: Option[Script]              = Option.apply(TxHelpers.script("true".stripMargin))
  val complexScriptAfter: Script                       = TxHelpers.script("false".stripMargin)
  val invokeFunctionName: String                       = "setData"
  val caller: String                                   = "i.caller"
  val originCaller: String                             = "i.originCaller"
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

  protected def createOrder(orderType: OrderType, orderSender: SeedKeyPair, orderVersion: Version): Order = {
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

  protected def checkAlias(append: Append, aliasTx: CreateAliasTransaction)(implicit pos: Position): Unit = {
    val firstTxParticipantBalanceAfter: Long = firstTxParticipantBalanceBefore - aliasTx.fee.value
    checkCreateAlias(append.transactionIds.head, append.transactionAt(0), aliasTx)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map((firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter))
    )
  }

  protected def checkTransferTx(append: Append, transferTx: TransferTransaction)(implicit pos: Position): Unit = {
    val firstTxParticipantBalanceAfter  = firstTxParticipantBalanceBefore - customFee - amount
    val secondTxParticipantBalanceAfter = secondTxParticipantBalanceBefore + amount
    checkTransfer(append.transactionIds.head, append.transactionAt(0), transferTx, secondTxParticipantAddress.publicKeyHash)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map(
        (firstTxParticipantAddress, Waves)  -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter),
        (secondTxParticipantAddress, Waves) -> (secondTxParticipantBalanceBefore, secondTxParticipantBalanceAfter)
      )
    )
  }

  protected def checkIssueTx(append: Append, issue: IssueTransaction, isNft: Boolean)(implicit pos: Position): Unit = {
    val assetDetails                   = append.transactionStateUpdates.head.assets.head
    val issueQuantity                  = issue.quantity.value
    val firstTxParticipantBalanceAfter = firstTxParticipantBalanceBefore - issue.fee.value

    checkIssue(append.transactionIds.head, append.transactionAt(0), issue)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
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

  protected def checkReissueTx(append: Append, reissue: ReissueTransaction, issue: IssueTransaction)(implicit pos: Position): Unit = {
    val assetDetails                           = append.transactionStateUpdates.head.assets.head
    val quantityAfter                          = amount + additionalAmount
    val firstTxParticipantBalanceBeforeReissue = firstTxParticipantBalanceBefore - issue.fee.value
    val firstTxParticipantBalanceAfterReissue  = firstTxParticipantBalanceBeforeReissue - reissue.fee.value

    checkReissue(append.transactionIds.head, append.transactionAt(0), reissue)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map(
        (firstTxParticipantAddress, Waves)         -> (firstTxParticipantBalanceBeforeReissue, firstTxParticipantBalanceAfterReissue),
        (firstTxParticipantAddress, reissue.asset) -> (amount, quantityAfter)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
    checkAssetsStateUpdates(assetDetails.after, reissue, isNft = false, quantityAfter)
  }

  protected def checkBurnTx(append: Append, burn: BurnTransaction, issue: IssueTransaction)(implicit pos: Position): Unit = {
    val assetDetails                        = append.transactionStateUpdates.head.assets.head
    val amountAfterTx                       = amount - additionalAmount
    val firstTxParticipantBalanceBeforeBurn = firstTxParticipantBalanceBefore - issue.fee.value
    val firstTxParticipantBalanceAfterBurn  = firstTxParticipantBalanceBeforeBurn - burn.fee.value

    checkBurn(append.transactionIds.head, append.transactionAt(0), burn)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map(
        (firstTxParticipantAddress, Waves)      -> (firstTxParticipantBalanceBeforeBurn, firstTxParticipantBalanceAfterBurn),
        (firstTxParticipantAddress, burn.asset) -> (amount, amountAfterTx)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
    checkAssetsStateUpdates(assetDetails.after, burn, isNft = false, amountAfterTx)
  }

  protected def checkExchangeTx(append: Append, exchangeTx: ExchangeTransaction, normalizedPrice: Long, orderAmount: Long)(implicit
      pos: Position
  ): Unit = {
    val amountAssetQuantity                      = secondToken.quantity.value
    val firstTxParticipantBalanceBeforeExchange  = firstTxParticipantBalanceBefore - firstTokenFee
    val firstTxParticipantBalanceAfterExchange   = firstTxParticipantBalanceBeforeExchange - exchangeTx.fee.value + customFee
    val secondTxParticipantBalanceBeforeExchange = secondTxParticipantBalanceBefore - secondToken.fee.value
    val secondTxParticipantBalanceAfterExchange  = secondTxParticipantBalanceBeforeExchange - customFee

    checkExchange(append.transactionIds.head, append.transactionAt(0), exchangeTx)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
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

  protected def checkLeaseTx(append: Append, lease: LeaseTransaction)(implicit pos: Position): Unit = {
    val leaseId = lease.id.value().arr
    checkLease(append.transactionIds.head, append.transactionAt(0), lease, secondTxParticipantPKHash)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
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

  protected def checkLeaseCancelTx(append: Append, leaseCancel: LeaseCancelTransaction, lease: LeaseTransaction)(implicit pos: Position): Unit = {
    val leaseId                           = leaseCancel.leaseId.arr
    val firstTxParticipantBalanceBeforeTx = firstTxParticipantBalanceBefore - lease.fee.value
    val firstTxParticipantBalanceAfterTx  = firstTxParticipantBalanceBeforeTx - leaseCancel.fee.value
    checkLeaseCancel(append.transactionIds.head, append.transactionAt(0), leaseCancel)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
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

  protected def checkForMassTransferTx(append: Append, massTransfer: MassTransferTransaction): Unit = {
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

  protected def checkDataTransfer(append: Append, data: DataTransaction)(implicit pos: Position): Unit = {
    val txUpdates = append.transactionStateUpdates.head
    checkDataTransaction(append.transactionIds.head, append.transactionAt(0), data)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map((firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceBefore - customFee))
    )
    checkDataEntriesStateUpdate(txUpdates.dataEntries, data.data, firstTxParticipantAddress.bytes)
  }

  protected def checkSetScript(append: Append, setScript: SetScriptTransaction)(implicit pos: Position): Unit = {
    val txUpdates = append.transactionStateUpdates.head
    checkSetScriptTransaction(append.transactionIds.head, append.transactionAt(0), setScript)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map((firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceBefore - customFee))
    )
    checkSetScriptStateUpdate(txUpdates.scripts.head, setScript)
  }

  protected def checkSetAssetScript(append: Append, setAssetScript: SetAssetScriptTransaction, issue: IssueTransaction)(implicit
      pos: Position
  ): Unit = {
    val quantity                                        = issue.quantity.value
    val firstTxParticipantBalanceBeforeSetAssetScriptTx = firstTxParticipantBalanceBefore - issue.fee.value
    val firstTxParticipantBalanceAfterSetAssetScriptTx  = firstTxParticipantBalanceBeforeSetAssetScriptTx - setAssetScript.fee.value
    val txUpdates                                       = append.transactionStateUpdates.head
    val assetDetails                                    = txUpdates.assets.head
    checkSetAssetScriptTransaction(append.transactionIds.head, append.transactionAt(0), setAssetScript)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map(
        (firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBeforeSetAssetScriptTx, firstTxParticipantBalanceAfterSetAssetScriptTx)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, quantity)
    checkAssetsScriptStateUpdates(assetDetails.before.get.scriptInfo, complexScriptBefore.get.bytes.value().arr)
    checkAssetsStateUpdates(assetDetails.after, issue, isNft = false, quantity)
    checkAssetsScriptStateUpdates(assetDetails.after.get.scriptInfo, complexScriptAfter.bytes.value().arr)
  }

  protected def checkUpdateAssetInfo(append: Append, updateAssetInfo: UpdateAssetInfoTransaction)(implicit pos: Position): Unit = {
    val firstTxParticipantBalanceBeforeUpdateAssetInfoTx = firstTxParticipantBalanceBefore - firstTokenFee
    val firstTxParticipantBalanceAfterUpdateAssetInfoTx  = firstTxParticipantBalanceBeforeUpdateAssetInfoTx - updateAssetInfo.fee
    val txUpdates                                        = append.transactionStateUpdates.head
    val assetDetails                                     = txUpdates.assets.head
    checkUpdateAssetInfoTransaction(append.transactionIds.head, append.transactionAt(0), updateAssetInfo)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map(
        (firstTxParticipantAddress, Waves) -> (firstTxParticipantBalanceBeforeUpdateAssetInfoTx, firstTxParticipantBalanceAfterUpdateAssetInfoTx)
      )
    )
    checkAssetsStateUpdates(assetDetails.before, firstToken, isNft = false, firstTokenQuantity)
    checkAssetUpdatesStateUpdates(assetDetails.after, updateAssetInfo)
  }

  protected def checkSponsorFee(append: Append, sponsorFee: SponsorFeeTransaction, balanceBefore: Long, balanceAfter: Long)(implicit
      pos: Position
  ): Unit = {
    val txUpdates    = append.transactionStateUpdates.head
    val assetDetails = txUpdates.assets.head

    checkSponsorFeeTransaction(append.transactionIds.head, append.transactionAt(0), sponsorFee)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map((firstTxParticipantAddress, Waves) -> (balanceBefore, balanceAfter))
    )
    checkAssetsStateUpdates(assetDetails.before, firstToken, isNft = false, firstTokenQuantity)
    checkAssetsStateUpdates(assetDetails.after, firstToken, isNft = false, firstTokenQuantity)
  }

  protected def checkEthereumTransfer(append: Append, ethereumTransfer: EthereumTransaction, ethAddress: Address)(implicit pos: Position): Unit = {
    val firstTxParticipantBalanceAfter = firstTxParticipantBalanceBefore - ethereumTransfer.fee
    val recipientTokenBalanceAfter     = secondTokenQuantity - amount

    checkEthereumTransaction(append.transactionIds.head, append.transactionAt(0), ethereumTransfer)
    checkBalances(
      filterOutMinerBalanceUpdates(append),
      Map(
        (ethAddress, Waves)                            -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter),
        (ethAddress, secondTokenAsset)                 -> (secondTokenQuantity, recipientTokenBalanceAfter),
        (secondTxParticipantAddress, secondTokenAsset) -> (0, amount)
      )
    )
  }

  def testInvoke(issue: IssueTransaction, invoke: Transaction, balances: Seq[AddrWithBalance])(
      checkType: BlockchainUpdateGrpcMethod,
      checkFunction: Append => Unit
  ): Unit = {
    for (libVersion <- 5 to 6) {
      val setScript = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
      checkType match {
        case Subscribe =>
          withGenerateSubscription(
            settings = currentSettings,
            balances = balances
          ) { d =>
            d.appendBlock(setScript)
            d.appendBlock(issue)
            d.appendMicroBlock(invoke)
          } { updates =>
            checkFunction(updates(3).append)
          }
        case GetBlockUpdate =>
          withGenerateGetBlockUpdate(
            height = 4,
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(setScript)
            d.appendBlock(issue)
            d.appendBlock(invoke)
          } { getBlockUpdate =>
            checkFunction(getBlockUpdate.getUpdate.getAppend)
          }
        case GetBlockUpdateRange =>
          withGenerateGetBlockUpdateRange(
            GetBlockUpdatesRangeRequest.of(1, 4),
            settings = currentSettings,
            balances = balances
          ) { d =>
            d.appendBlock(setScript)
            d.appendBlock(issue)
            d.appendBlock(invoke)
            d.appendBlock()
          } { getBlockUpdateRange =>
            checkFunction(getBlockUpdateRange.apply(3).getAppend)
          }
      }
    }
  }

  def doubleNestedInvokeTest(
      assetDappAccount: SeedKeyPair,
      balances: Seq[AddrWithBalance],
      issue: IssueTransaction,
      invoke: Transaction,
      massTx: Transaction,
      callerType: String,
      checkType: BlockchainUpdateGrpcMethod
  )(f: Append => Unit): Unit = {
    for (libVersion <- 5 to 6) {
      val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
      val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript(callerType, libVersion)))
      val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript(callerType, libVersion)))
      checkType match {
        case Subscribe =>
          withGenerateSubscription(
            SubscribeRequest.of(1, Int.MaxValue),
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
            d.appendMicroBlock(invoke)
          } { updates =>
            f(updates(2).getAppend)
          }

        case GetBlockUpdate =>
          withGenerateGetBlockUpdate(
            height = 3,
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
            d.appendBlock(invoke)
          } { getBlockUpdate =>
            f(getBlockUpdate.getUpdate.getAppend)
          }

        case GetBlockUpdateRange =>
          withGenerateGetBlockUpdateRange(
            GetBlockUpdatesRangeRequest.of(1, 3),
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
            d.appendBlock(invoke)
            d.appendBlock()
          } { getBlockUpdateRange =>
            f(getBlockUpdateRange.apply(2).getAppend)
          }
      }
    }
  }



  def checkGeneralInvoke(
      append: Append,
      issuerAssetBalanceAfterTx: Long,
      invoke: Transaction,
      issue: IssueTransaction,
      invokeScript: InvokeScriptMetadata,
      expectBalancesMap: Map[(Address, Asset), (Long, Long)]
  )(implicit pos: Position): Unit = {
    val issuerInvokeIssueBalance: Long = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
    val result                         = invokeScript.result.get
    val invokeIssueAsset               = toVanillaAssetId(result.issues.head.assetId)
    val expectMap = Map(
      (firstTxParticipantAddress, invokeIssueAsset)  -> (0L, issuerInvokeIssueBalance),
      (secondTxParticipantAddress, invokeIssueAsset) -> (0L, scriptTransferIssueAssetNum)
    ) ++ expectBalancesMap
    checkSimpleInvoke(append, issue, issuerAssetBalanceAfterTx, invokeScript, invoke.id.value().arr)

    filterOutMinerBalanceUpdates(append) should matchBalances(expectMap)
  }
}

object BlockchainUpdatesTestBase extends Assertions {
  def filterOutMinerBalanceUpdates(append: Append): Seq[StateUpdate.BalanceUpdate] = {
    val generatorPK = append.body match {
      case Body.Empty => fail("Append event is empty")
      case Body.Block(value) => value.getBlock.getHeader.generator
      case Body.MicroBlock(value) => value.getMicroBlock.getMicroBlock.senderPublicKey
    }
    val generatorAddress = ByteString.copyFrom(PublicKey(generatorPK.toByteArray).toAddress.bytes)

    append.transactionStateUpdates.head.balances.filterNot(_.address == generatorAddress)
  }
}
