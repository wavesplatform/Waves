package com.wavesplatform.protobuf.transaction
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.ExchangeTransactionData.{BuySellOrders, Orders}
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.protobuf.transaction.{Script => PBScript}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.smart.script.ScriptReader
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Asset, Proofs, ValidationError}
import com.wavesplatform.{transaction => vt}

object PBTransactions {
  import com.wavesplatform.protobuf.utils.PBInternalImplicits._

  private[this] val NoChainId: Byte = 0: Byte

  def create(sender: com.wavesplatform.account.PublicKeyAccount = PublicKeyAccount.empty,
             chainId: Byte = 0,
             fee: Long = 0L,
             feeAssetId: VanillaAssetId = Waves,
             timestamp: Long = 0L,
             version: Int = 0,
             proofsArray: Seq[com.wavesplatform.common.state.ByteStr] = Nil,
             data: com.wavesplatform.protobuf.transaction.Transaction.Data = com.wavesplatform.protobuf.transaction.Transaction.Data.Empty)
    : SignedTransaction = {
    new SignedTransaction(
      Some(Transaction(chainId, sender.publicKey: ByteStr, Some((feeAssetId, fee): Amount), timestamp, version, data)),
      proofsArray.map(bs => ByteString.copyFrom(bs.arr))
    )
  }

  def vanilla(signedTx: PBSignedTransaction, unsafe: Boolean = false): Either[ValidationError, VanillaTransaction] = {
    def toAmountAndAssetId(amount: Amount): Either[ValidationError, (Long, VanillaAssetId)] = amount.amount match {
      case Amount.Amount.WavesAmount(value)                        => Right((value, Waves))
      case Amount.Amount.AssetAmount(AssetAmount(assetId, amount)) => Right((amount, IssuedAsset(assetId.toByteArray)))
      case Amount.Amount.Empty                                     => Left(GenericError("Empty amount"))
    }

    for {
      parsedTx  <- signedTx.transaction.toRight(GenericError("Transaction must be specified"))
      fee       <- parsedTx.fee.toRight(GenericError("Fee must be specified"))
      _         <- Either.cond(parsedTx.data.isDefined, (), GenericError("Transaction data must be specified"))
      feeAmount <- toAmountAndAssetId(fee)
      sender = PublicKeyAccount(parsedTx.senderPublicKey.toByteArray)
      tx <- if (unsafe)
        Right(
          createVanillaUnsafe(
            parsedTx.version,
            parsedTx.chainId.toByte,
            sender,
            feeAmount._1,
            feeAmount._2,
            parsedTx.timestamp,
            Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
            parsedTx.data
          ))
      else
        createVanilla(
          parsedTx.version,
          parsedTx.chainId.toByte,
          sender,
          feeAmount._1,
          feeAmount._2,
          parsedTx.timestamp,
          Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
          parsedTx.data
        )
    } yield tx
  }

  private[this] def createVanilla(version: Int,
                                  chainId: Byte,
                                  sender: PublicKeyAccount,
                                  feeAmount: Long,
                                  feeAssetId: VanillaAssetId,
                                  timestamp: Long,
                                  proofs: Proofs,
                                  data: PBTransaction.Data): Either[ValidationError, VanillaTransaction] = {

    val signature = proofs.toSignature
    val result: Either[ValidationError, VanillaTransaction] = data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction.create(Address.fromBytes(recipient.toByteArray).right.get, amount, timestamp)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction.create(sender, Address.fromBytes(recipient.toByteArray).right.get, amount, feeAmount, timestamp, signature)

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        version match {
          case 1 =>
            for {
              address <- recipient.toAddressOrAlias
              tx <- vt.transfer.TransferTransactionV1.create(
                amount.assetId,
                sender,
                address,
                amount.longAmount,
                timestamp,
                feeAssetId,
                feeAmount,
                attachment.toByteArray,
                signature
              )
            } yield tx

          case 2 =>
            for {
              address <- recipient.toAddressOrAlias
              tx <- vt.transfer.TransferTransactionV2.create(
                amount.assetId,
                sender,
                address,
                amount.longAmount,
                timestamp,
                feeAssetId,
                feeAmount,
                attachment.toByteArray,
                proofs
              )
            } yield tx

          case v =>
            throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        version match {
          case 1 =>
            for {
              alias <- com.wavesplatform.account.Alias.createWithChainId(alias, chainId)
              tx    <- vt.CreateAliasTransactionV1.create(sender, alias, feeAmount, timestamp, signature)
            } yield tx

          case 2 =>
            for {
              alias <- com.wavesplatform.account.Alias.createWithChainId(alias, chainId)
              tx    <- vt.CreateAliasTransactionV2.create(sender, alias, feeAmount, timestamp, proofs)
            } yield tx

          case v =>
            throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        version match {
          case 1 =>
            vt.assets.IssueTransactionV1.create(
              sender,
              name.toByteArray,
              description.toByteArray,
              quantity,
              decimals.toByte,
              reissuable,
              feeAmount,
              timestamp,
              signature
            )
          case 2 =>
            vt.assets.IssueTransactionV2.create(
              chainId,
              sender,
              name.toByteArray,
              description.toByteArray,
              quantity,
              decimals.toByte,
              reissuable,
              script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
              feeAmount,
              timestamp,
              proofs
            )
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Reissue(ReissueTransactionData(Some(AssetAmount(assetId, amount)), reissuable)) =>
        version match {
          case 1 =>
            vt.assets.ReissueTransactionV1.create(sender, IssuedAsset(assetId.toByteArray), amount, reissuable, feeAmount, timestamp, signature)
          case 2 =>
            vt.assets.ReissueTransactionV2.create(chainId, sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Burn(BurnTransactionData(Some(AssetAmount(assetId, amount)))) =>
        version match {
          case 1 => vt.assets.BurnTransactionV1.create(sender, IssuedAsset(assetId), amount, feeAmount, timestamp, signature)
          case 2 => vt.assets.BurnTransactionV2.create(chainId, sender, IssuedAsset(assetId), amount, feeAmount, timestamp, proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction.create(
          chainId,
          sender,
          IssuedAsset(assetId),
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction.create(
          sender,
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        version match {
          case 1 =>
            for {
              address <- recipient.toAddressOrAlias
              tx      <- vt.lease.LeaseTransactionV1.create(sender, amount, feeAmount, timestamp, address, signature)
            } yield tx

          case 2 =>
            for {
              address <- recipient.toAddressOrAlias
              tx      <- vt.lease.LeaseTransactionV2.create(sender, amount, feeAmount, timestamp, address, proofs)
            } yield tx

          case v =>
            throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        version match {
          case 1 => vt.lease.LeaseCancelTransactionV1.create(sender, leaseId.byteStr, feeAmount, timestamp, signature)
          case 2 => vt.lease.LeaseCancelTransactionV2.create(chainId, sender, leaseId.toByteArray, feeAmount, timestamp, proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Exchange(
          ExchangeTransactionData(amount,
                                  price,
                                  buyMatcherFee,
                                  sellMatcherFee,
                                  Orders.BuySellOrders(BuySellOrders(Some(buyOrder), Some(sellOrder))))) =>
        version match {
          case 1 =>
            vt.assets.exchange.ExchangeTransactionV1.create(
              PBOrders.vanillaV1(buyOrder),
              PBOrders.vanillaV1(sellOrder),
              amount,
              price,
              buyMatcherFee,
              sellMatcherFee,
              feeAmount,
              timestamp,
              signature
            )
          case 2 =>
            vt.assets.exchange.ExchangeTransactionV2.create(PBOrders.vanilla(buyOrder),
                                                            PBOrders.vanilla(sellOrder),
                                                            amount,
                                                            price,
                                                            buyMatcherFee,
                                                            sellMatcherFee,
                                                            feeAmount,
                                                            timestamp,
                                                            proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.DataTransaction(DataTransactionData(data)) =>
        import DataTransactionData.DataEntry.Value._
        val entries = data.toList.map { de =>
          de.value match {
            case IntValue(num)      => IntegerDataEntry(de.key, num)
            case BoolValue(bool)    => BooleanDataEntry(de.key, bool)
            case BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteArray)
            case StringValue(str)   => StringDataEntry(de.key, str)
            case Empty              => throw new IllegalArgumentException(s"Empty entries not supported: $data")
          }
        }
        vt.DataTransaction.create(
          sender,
          entries,
          feeAmount,
          timestamp,
          proofs
        )

      case Data.MassTransfer(MassTransferTransactionData(assetId, transfers, attachment)) =>
        vt.transfer.MassTransferTransaction.create(
          Asset.fromCompatId(Option(assetId.toByteArray: ByteStr).filterNot(_.isEmpty)),
          sender,
          transfers.flatMap(t => t.getAddress.toAddressOrAlias.toOption.map(ParsedTransfer(_, t.amount))).toList,
          timestamp,
          feeAmount,
          attachment.toByteArray,
          proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(AssetAmount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction.create(sender, IssuedAsset(assetId), Option(minFee).filter(_ > 0), feeAmount, timestamp, proofs)

      case data =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $data")
    }

    result
  }

  private[this] def createVanillaUnsafe(version: Int,
                                        chainId: Byte,
                                        sender: PublicKeyAccount,
                                        feeAmount: Long,
                                        feeAssetId: VanillaAssetId,
                                        timestamp: Long,
                                        proofs: Proofs,
                                        data: PBTransaction.Data): VanillaTransaction = {
    import com.wavesplatform.common.utils._

    val signature = proofs.toSignature
    data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction(Address.fromBytes(recipient.toByteArray).right.get, amount, timestamp, signature)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction(sender, Address.fromBytes(recipient.toByteArray).right.get, amount, feeAmount, timestamp, signature)

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        version match {
          case 1 =>
            vt.transfer.TransferTransactionV1(
              amount.assetId,
              sender,
              recipient.toAddressOrAlias.explicitGet(),
              amount.longAmount,
              timestamp,
              feeAssetId,
              feeAmount,
              attachment.toByteArray,
              signature
            )

          case 2 =>
            vt.transfer.TransferTransactionV2(
              sender,
              recipient.toAddressOrAlias.explicitGet(),
              amount.assetId,
              amount.longAmount,
              timestamp,
              feeAssetId,
              feeAmount,
              attachment.toByteArray,
              proofs
            )

          case v =>
            throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        version match {
          case 1 =>
            vt.CreateAliasTransactionV1(sender,
                                        com.wavesplatform.account.Alias.createWithChainId(alias, chainId).explicitGet(),
                                        feeAmount,
                                        timestamp,
                                        signature)

          case 2 =>
            vt.CreateAliasTransactionV2(sender,
                                        com.wavesplatform.account.Alias.createWithChainId(alias, chainId).explicitGet(),
                                        feeAmount,
                                        timestamp,
                                        proofs)

          case v =>
            throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        version match {
          case 1 =>
            vt.assets.IssueTransactionV1(
              sender,
              name.toByteArray,
              description.toByteArray,
              quantity,
              decimals.toByte,
              reissuable,
              feeAmount,
              timestamp,
              signature
            )
          case 2 =>
            vt.assets.IssueTransactionV2(
              chainId,
              sender,
              name.toByteArray,
              description.toByteArray,
              quantity,
              decimals.toByte,
              reissuable,
              script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
              feeAmount,
              timestamp,
              proofs
            )
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Reissue(ReissueTransactionData(Some(AssetAmount(assetId, amount)), reissuable)) =>
        version match {
          case 1 =>
            vt.assets.ReissueTransactionV1(sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, signature)
          case 2 =>
            vt.assets.ReissueTransactionV2(chainId, sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Burn(BurnTransactionData(Some(AssetAmount(assetId, amount)))) =>
        version match {
          case 1 => vt.assets.BurnTransactionV1(sender, IssuedAsset(assetId), amount, feeAmount, timestamp, signature)
          case 2 => vt.assets.BurnTransactionV2(chainId, sender, IssuedAsset(assetId), amount, feeAmount, timestamp, proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(
          chainId,
          sender,
          IssuedAsset(assetId),
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(
          chainId,
          sender,
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        version match {
          case 1 =>
            vt.lease.LeaseTransactionV1(sender, amount, feeAmount, timestamp, recipient.toAddressOrAlias.explicitGet(), signature)

          case 2 =>
            vt.lease.LeaseTransactionV2(sender, amount, feeAmount, timestamp, recipient.toAddressOrAlias.explicitGet(), proofs)

          case v =>
            throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        version match {
          case 1 => vt.lease.LeaseCancelTransactionV1(sender, leaseId.byteStr, feeAmount, timestamp, signature)
          case 2 => vt.lease.LeaseCancelTransactionV2(chainId, sender, leaseId.toByteArray, feeAmount, timestamp, proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Exchange(
          ExchangeTransactionData(amount,
                                  price,
                                  buyMatcherFee,
                                  sellMatcherFee,
                                  Orders.BuySellOrders(BuySellOrders(Some(buyOrder), Some(sellOrder))))) =>
        version match {
          case 1 =>
            vt.assets.exchange.ExchangeTransactionV1(
              PBOrders.vanillaV1(buyOrder),
              PBOrders.vanillaV1(sellOrder),
              amount,
              price,
              buyMatcherFee,
              sellMatcherFee,
              feeAmount,
              timestamp,
              signature
            )
          case 2 =>
            vt.assets.exchange.ExchangeTransactionV2(PBOrders.vanilla(buyOrder),
                                                     PBOrders.vanilla(sellOrder),
                                                     amount,
                                                     price,
                                                     buyMatcherFee,
                                                     sellMatcherFee,
                                                     feeAmount,
                                                     timestamp,
                                                     proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.DataTransaction(DataTransactionData(data)) =>
        import DataTransactionData.DataEntry.Value._
        val entries = data.toList.map { de =>
          de.value match {
            case IntValue(num)      => IntegerDataEntry(de.key, num)
            case BoolValue(bool)    => BooleanDataEntry(de.key, bool)
            case BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteArray)
            case StringValue(str)   => StringDataEntry(de.key, str)
            case Empty              => throw new IllegalArgumentException(s"Empty entries not supported: $data")
          }
        }
        vt.DataTransaction(
          sender,
          entries,
          feeAmount,
          timestamp,
          proofs
        )

      case Data.MassTransfer(MassTransferTransactionData(assetId, transfers, attachment)) =>
        vt.transfer.MassTransferTransaction(
          Asset.fromProtoId(assetId),
          sender,
          transfers.flatMap(t => t.getAddress.toAddressOrAlias.toOption.map(ParsedTransfer(_, t.amount))).toList,
          timestamp,
          feeAmount,
          attachment.toByteArray,
          proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(AssetAmount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction(sender, IssuedAsset(assetId), Option(minFee).filter(_ > 0), feeAmount, timestamp, proofs)

      case data =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $data")
    }
  }

  def protobuf(tx: VanillaTransaction): PBSignedTransaction = {
    tx match {
      // Uses version "2" for "modern" transactions with single version and proofs field
      case vt.GenesisTransaction(recipient, amount, timestamp, signature) =>
        val data = GenesisTransactionData(ByteString.copyFrom(recipient.bytes), amount)
        PBTransactions.create(sender = PublicKeyAccount(Array.emptyByteArray), timestamp = timestamp, version = 1, data = Data.Genesis(data))

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature) =>
        val data = PaymentTransactionData(ByteString.copyFrom(recipient.bytes), amount)
        PBTransactions.create(sender, NoChainId, fee, Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case vt.transfer.TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, fee, attachment, signature) =>
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), ByteString.copyFrom(attachment))
        PBTransactions.create(sender, NoChainId, fee, feeAssetId, timestamp, 1, Seq(signature), Data.Transfer(data))

      case vt.transfer.TransferTransactionV2(sender, recipient, assetId, amount, timestamp, feeAssetId, fee, attachment, proofs) =>
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), ByteString.copyFrom(attachment))
        PBTransactions.create(sender, NoChainId, fee, feeAssetId, timestamp, 2, proofs, Data.Transfer(data))

      case tx @ vt.CreateAliasTransactionV1(sender, alias, fee, timestamp, signature) =>
        val data = CreateAliasTransactionData(alias.name)
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.CreateAlias(data))

      case tx @ vt.CreateAliasTransactionV2(sender, alias, fee, timestamp, proofs) =>
        val data = CreateAliasTransactionData(alias.name)
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.CreateAlias(data))

      case tx @ vt.assets.exchange
            .ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature) =>
        val data = ExchangeTransactionData(
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          Orders.BuySellOrders(BuySellOrders(Some(PBOrders.protobuf(buyOrder)), Some(PBOrders.protobuf(sellOrder))))
        )
        PBTransactions.create(tx.sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Exchange(data))

      case tx @ vt.assets.exchange.ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) =>
        val data = ExchangeTransactionData(
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          Orders.BuySellOrders(BuySellOrders(Some(PBOrders.protobuf(buyOrder)), Some(PBOrders.protobuf(sellOrder))))
        )
        PBTransactions.create(tx.sender, 0: Byte, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Exchange(data))

      case vt.assets.IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, None)
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Issue(data))

      case vt.assets.IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, script.map(s => PBScript(s.bytes())))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Issue(data))

      case tx @ vt.assets.ReissueTransactionV1(sender, assetId, quantity, reissuable, fee, timestamp, signature) =>
        val data = ReissueTransactionData(Some(AssetAmount(assetId.id, quantity)), reissuable)
        PBTransactions.create(sender, tx.chainByte.getOrElse(NoChainId), fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Reissue(data))

      case tx @ vt.assets.ReissueTransactionV2(chainId, sender, assetId, amount, reissuable, fee, timestamp, proofs) =>
        val data = ReissueTransactionData(Some(AssetAmount(assetId.id, amount)), reissuable)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Reissue(data))

      case tx @ vt.assets.BurnTransactionV1(sender, assetId, amount, fee, timestamp, signature) =>
        val data = BurnTransactionData(Some(AssetAmount(assetId.id, amount)))
        PBTransactions.create(sender, tx.chainByte.getOrElse(NoChainId), fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Burn(data))

      case tx @ vt.assets.BurnTransactionV2(chainId, sender, assetId, amount, fee, timestamp, proofs) =>
        val data = BurnTransactionData(Some(AssetAmount(assetId.id, amount)))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Burn(data))

      case vt.assets.SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(assetId.id, script.map(s => PBScript(s.bytes())))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SetAssetScript(data))

      case vt.smart.SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script.map(s => PBScript(s.bytes())))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SetScript(data))

      case tx @ vt.lease.LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature) =>
        val data = LeaseTransactionData(Some(recipient), amount)
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Lease(data))

      case tx @ vt.lease.LeaseTransactionV2(sender, amount, fee, timestamp, recipient, proofs) =>
        val data = LeaseTransactionData(Some(recipient), amount)
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Lease(data))

      case tx @ vt.lease.LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature) =>
        val data = LeaseCancelTransactionData(leaseId)
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.LeaseCancel(data))

      case tx @ vt.lease.LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs) =>
        val data = LeaseCancelTransactionData(leaseId)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferTransactionData(
          ByteString.copyFrom(assetId.compatId.getOrElse(ByteStr.empty)),
          transfers.map(pt => MassTransferTransactionData.Transfer(Some(pt.address), pt.amount)),
          attachment: ByteStr
        )
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.MassTransfer(data))

      case tx @ vt.DataTransaction(sender, data, fee, timestamp, proofs) =>
        val txData = DataTransactionData(
          data.map(de =>
            DataTransactionData.DataEntry(
              de.key,
              de match {
                case IntegerDataEntry(_, value) => DataTransactionData.DataEntry.Value.IntValue(value)
                case BooleanDataEntry(_, value) => DataTransactionData.DataEntry.Value.BoolValue(value)
                case BinaryDataEntry(_, value)  => DataTransactionData.DataEntry.Value.BinaryValue(value)
                case StringDataEntry(_, value)  => DataTransactionData.DataEntry.Value.StringValue(value)
              }
          )))
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(Some(AssetAmount(assetId.id, minSponsoredAssetFee.getOrElse(0L))))
        PBTransactions.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SponsorFee(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }
}
