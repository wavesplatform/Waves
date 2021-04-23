package com.wavesplatform.protobuf.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Proofs, TxValidationError}
import com.wavesplatform.utils.StringBytes
import com.wavesplatform.{transaction => vt}

import scala.util.Try

object PBTransactions {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def createGenesis(chainId: Byte, timestamp: Long, signature: ByteStr, data: GenesisTransactionData): SignedTransaction =
    new SignedTransaction(
      Some(Transaction(chainId, timestamp = timestamp, data = Data.Genesis(data))),
      Seq(ByteString.copyFrom(signature.arr))
    )

  def create(
      sender: com.wavesplatform.account.PublicKey,
      chainId: Byte = 0,
      fee: Long = 0L,
      feeAssetId: VanillaAssetId = Waves,
      timestamp: Long = 0L,
      version: Int = 0,
      proofsArray: Seq[com.wavesplatform.common.state.ByteStr] = Nil,
      data: com.wavesplatform.protobuf.transaction.Transaction.Data = com.wavesplatform.protobuf.transaction.Transaction.Data.Empty
  ): SignedTransaction =
    new SignedTransaction(
      Some(Transaction(chainId, sender.toByteString, Some((feeAssetId, fee): Amount), timestamp, version, data)),
      proofsArray.map(bs => ByteString.copyFrom(bs.arr))
    )

  def vanillaUnsafe(signedTx: PBSignedTransaction): VanillaTransaction = {
    import com.wavesplatform.common.utils._
    vanilla(signedTx, unsafe = true).explicitGet()
  }

  def tryToVanilla(signedTx: PBSignedTransaction): Try[VanillaTransaction] =
    vanilla(signedTx).left.map(err => new Exception(err.toString)).toTry

  def vanilla(signedTx: PBSignedTransaction, unsafe: Boolean = false): Either[ValidationError, VanillaTransaction] = {
    for {
      parsedTx <- signedTx.transaction.toRight(GenericError("Transaction must be specified"))
      fee = parsedTx.fee.getOrElse(Amount.defaultInstance)
      _ <- Either.cond(parsedTx.data.isDefined, (), GenericError("Transaction data must be specified"))
      feeAmount = PBAmounts.toAssetAndAmount(fee)
      sender = Option(parsedTx.senderPublicKey)
        .filterNot(_.isEmpty)
        .map(pk => PublicKey(pk.toByteArray))
        .orNull
      tx <- if (unsafe)
        Right(
          createVanillaUnsafe(
            parsedTx.version,
            parsedTx.chainId.toByte,
            sender,
            feeAmount._2,
            feeAmount._1,
            parsedTx.timestamp,
            Proofs(signedTx.proofs.map(_.toByteStr)),
            parsedTx.data
          )
        )
      else
        for {
          proofs <- Proofs.create(signedTx.proofs.map(_.toByteStr))
          tx <- createVanilla(
            parsedTx.version,
            parsedTx.chainId.toByte,
            sender,
            feeAmount._2,
            feeAmount._1,
            parsedTx.timestamp,
            proofs,
            parsedTx.data
          )
        } yield tx
    } yield tx
  }

  private[this] def createVanilla(
      version: Int,
      chainId: Byte,
      sender: PublicKey,
      feeAmount: Long,
      feeAssetId: VanillaAssetId,
      timestamp: Long,
      proofs: Proofs,
      data: PBTransaction.Data
  ): Either[ValidationError, VanillaTransaction] = {

    val signature = proofs.toSignature
    val result: Either[ValidationError, VanillaTransaction] = data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        for {
          addr <- PBRecipients.toAddress(recipient.toByteArray, chainId)
          tx   <- vt.GenesisTransaction.create(addr, amount, timestamp)
        } yield tx

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        for {
          addr <- PBRecipients.toAddress(recipient.toByteArray, chainId)
          tx   <- vt.PaymentTransaction.create(sender, addr, amount, feeAmount, timestamp, signature)
        } yield tx

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        for {
          address <- recipient.toAddressOrAlias(chainId)
          tx <- vt.transfer.TransferTransaction.create(
            version.toByte,
            sender,
            address,
            amount.vanillaAssetId,
            amount.longAmount,
            feeAssetId,
            feeAmount,
            attachment.toByteStr,
            timestamp,
            proofs
          )
        } yield tx

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        for {
          tx <- vt.CreateAliasTransaction.create(version.toByte, sender, alias, feeAmount, timestamp, Proofs(Seq(signature)), chainId)
        } yield tx

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        vt.assets.IssueTransaction.create(
          version.toByte,
          sender,
          name,
          description,
          quantity,
          decimals.toByte,
          reissuable,
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount)), reissuable)) =>
        vt.assets.ReissueTransaction.create(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          amount,
          reissuable,
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount)))) =>
        vt.assets.BurnTransaction.create(version.toByte, sender, IssuedAsset(assetId.toByteStr), amount, feeAmount, timestamp, proofs, chainId)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction.create(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction.create(
          version.toByte,
          sender,
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        for {
          address <- recipient.toAddressOrAlias(chainId)
          tx      <- vt.lease.LeaseTransaction.create(version.toByte, sender, address, amount, feeAmount, timestamp, proofs)
        } yield tx

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        vt.lease.LeaseCancelTransaction.create(version.toByte, sender, leaseId.toByteStr, feeAmount, timestamp, proofs, chainId)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(order1, order2))) =>
        vt.assets.exchange.ExchangeTransaction.create(
          version.toByte,
          PBOrders.vanilla(order1),
          PBOrders.vanilla(order2),
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction.create(version.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs, chainId)

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction.create(
          version.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers.flatMap(t => t.getRecipient.toAddressOrAlias(chainId).toOption.map(ParsedTransfer(_, t.amount))).toList,
          feeAmount,
          timestamp,
          mt.attachment.toByteStr,
          proofs,
          chainId
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction.create(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          Option(minFee).filter(_ > 0),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments)) =>
        import cats.instances.either._
        import cats.instances.option._
        import cats.syntax.traverse._
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL

        for {
          dApp <- PBRecipients.toAddressOrAlias(dappAddress, chainId)

          fcOpt <- Deser
            .parseOption(functionCall.asReadOnlyByteBuffer())(Serde.deserialize)
            .sequence
            .left
            .map(e => GenericError(s"Invalid InvokeScript function call: $e"))

          _ <- fcOpt match {
            case None | Some(Terms.FUNCTION_CALL(_, _)) => Right(())
            case Some(expr)                             => Left(GenericError(s"Not a function call: $expr"))
          }

          tx <- vt.smart.InvokeScriptTransaction.create(
            version.toByte,
            sender,
            dApp,
            fcOpt.map(_.asInstanceOf[FUNCTION_CALL]),
            payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
            feeAmount,
            feeAssetId,
            timestamp,
            proofs
          )
        } yield tx

      case Data.UpdateAssetInfo(UpdateAssetInfoTransactionData(assetId, name, description)) =>
        UpdateAssetInfoTransaction.create(
          version.toByte,
          sender,
          assetId.toByteStr,
          name,
          description,
          timestamp,
          feeAmount,
          feeAssetId,
          proofs,
          chainId
        )

      case _ =>
        Left(TxValidationError.UnsupportedTransactionType)
    }

    result
  }

  private[this] def createVanillaUnsafe(
      version: Int,
      chainId: Byte,
      sender: PublicKey,
      feeAmount: Long,
      feeAssetId: VanillaAssetId,
      timestamp: Long,
      proofs: Proofs,
      data: PBTransaction.Data
  ): VanillaTransaction = {
    import com.wavesplatform.common.utils._

    val signature = proofs.toSignature
    data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction(PBRecipients.toAddress(recipient.toByteArray, chainId).explicitGet(), amount, timestamp, signature, chainId)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction(
          sender,
          PBRecipients.toAddress(recipient.toByteArray, chainId).explicitGet(),
          amount,
          feeAmount,
          timestamp,
          signature,
          chainId
        )

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        vt.transfer.TransferTransaction(
          version.toByte,
          sender,
          recipient.toAddressOrAlias(chainId).explicitGet(),
          amount.vanillaAssetId,
          amount.longAmount,
          feeAssetId,
          feeAmount,
          attachment.toByteStr,
          timestamp,
          proofs,
          chainId
        )

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        vt.CreateAliasTransaction(
          version.toByte,
          sender,
          alias,
          feeAmount,
          timestamp,
          Proofs(signature),
          chainId
        )

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        vt.assets.IssueTransaction(
          version.toByte,
          sender,
          name.toByteString,
          description.toByteString,
          quantity,
          decimals.toByte,
          reissuable,
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount)), reissuable)) =>
        vt.assets.ReissueTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          amount,
          reissuable,
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount)))) =>
        vt.assets.BurnTransaction(version.toByte, sender, IssuedAsset(assetId.toByteStr), amount, feeAmount, timestamp, proofs, chainId)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(
          version.toByte,
          sender,
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        vt.lease.LeaseTransaction(
          version.toByte,
          sender,
          recipient.toAddressOrAlias(chainId).explicitGet(),
          amount,
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        vt.lease.LeaseCancelTransaction(version.toByte, sender, leaseId.toByteStr, feeAmount, timestamp, proofs, chainId)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(buyOrder, sellOrder))) =>
        vt.assets.exchange.ExchangeTransaction(
          version.toByte,
          PBOrders.vanilla(buyOrder),
          PBOrders.vanilla(sellOrder),
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction(version.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs, chainId)

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction(
          version.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers.flatMap(t => t.getRecipient.toAddressOrAlias(chainId).toOption.map(ParsedTransfer(_, t.amount))).toList,
          feeAmount,
          timestamp,
          mt.attachment.toByteStr,
          proofs,
          chainId
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          Option(minFee).filter(_ > 0),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments)) =>
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL

        vt.smart.InvokeScriptTransaction(
          version.toByte,
          sender,
          PBRecipients.toAddressOrAlias(dappAddress, chainId).explicitGet(),
          Deser
            .parseOption(functionCall.asReadOnlyByteBuffer())(Serde.deserialize)
            .map(_.explicitGet().asInstanceOf[FUNCTION_CALL]),
          payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
          feeAmount,
          feeAssetId,
          timestamp,
          proofs,
          chainId
        )

      case Data.UpdateAssetInfo(UpdateAssetInfoTransactionData(assetId, name, description)) =>
        vt.assets.UpdateAssetInfoTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          name,
          description,
          timestamp,
          feeAmount,
          feeAssetId,
          proofs,
          chainId
        )

      case other =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $other")
    }
  }

  def protobuf(tx: VanillaTransaction): PBSignedTransaction = {
    tx match {
      case vt.GenesisTransaction(recipient, amount, timestamp, signature, chainId) =>
        PBTransactions.createGenesis(
          chainId,
          timestamp,
          signature,
          GenesisTransactionData(PBRecipients.create(recipient).getPublicKeyHash, amount)
        )

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature, chainId) =>
        val data = PaymentTransactionData(PBRecipients.create(recipient).getPublicKeyHash, amount)
        PBTransactions.create(sender, chainId, fee, Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case tx: vt.transfer.TransferTransaction =>
        import tx._
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), attachment.toByteString)
        PBTransactions.create(sender, chainId, fee, feeAssetId, timestamp, version, proofs, Data.Transfer(data))

      case tx: vt.CreateAliasTransaction =>
        import tx._
        val data = CreateAliasTransactionData(alias.name)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.CreateAlias(data))

      case tx: vt.assets.exchange.ExchangeTransaction =>
        import tx._
        val data = ExchangeTransactionData(
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          Seq(PBOrders.protobuf(order1), PBOrders.protobuf(order2))
        )
        PBTransactions.create(tx.sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Exchange(data))

      case tx: vt.assets.IssueTransaction =>
        import tx._
        val data = IssueTransactionData(name.toStringUtf8, description.toStringUtf8, quantity, decimals, reissuable, toPBScript(script))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Issue(data))

      case tx: vt.assets.ReissueTransaction =>
        import tx._
        val data = ReissueTransactionData(Some(Amount(asset.id.toByteString, quantity)), reissuable)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Reissue(data))

      case tx: vt.assets.BurnTransaction =>
        import tx._
        val data = BurnTransactionData(Some(Amount(asset.id.toByteString, quantity)))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Burn(data))

      case tx @ vt.assets.SetAssetScriptTransaction(_, sender, assetId, script, fee, timestamp, proofs, chainId) =>
        val data = SetAssetScriptTransactionData(assetId.id.toByteString, toPBScript(script))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, tx.version, proofs, Data.SetAssetScript(data))

      case tx @ vt.smart.SetScriptTransaction(_, sender, script, fee, timestamp, proofs, chainId) =>
        val data = SetScriptTransactionData(toPBScript(script))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, tx.version, proofs, Data.SetScript(data))

      case tx: vt.lease.LeaseTransaction =>
        import tx._
        val data = LeaseTransactionData(Some(recipient), amount)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Lease(data))

      case tx: vt.lease.LeaseCancelTransaction =>
        import tx._
        val data = LeaseCancelTransactionData(leaseId.toByteString)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(version, sender, assetId, transfers, fee, timestamp, attachment, proofs, chainId) =>
        val data = MassTransferTransactionData(
          PBAmounts.toPBAssetId(assetId),
          transfers.map(pt => MassTransferTransactionData.Transfer(Some(pt.address), pt.amount)),
          attachment.toByteString
        )
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.MassTransfer(data))

      case tx @ vt.DataTransaction(version, sender, data, fee, timestamp, proofs, chainId) =>
        val txData = DataTransactionData(data.map(toPBDataEntry))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(version, sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs, chainId) =>
        val data = SponsorFeeTransactionData(Some(Amount(assetId.id.toByteString, minSponsoredAssetFee.getOrElse(0L))))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.SponsorFee(data))

      case vt.smart.InvokeScriptTransaction(version, sender, dappAddress, fcOpt, payment, fee, feeAssetId, timestamp, proofs, chainId) =>
        val data = Data.InvokeScript(toPBInvokeScriptData(dappAddress, fcOpt, payment))
        PBTransactions.create(sender, chainId, fee, feeAssetId, timestamp, version, proofs, data)

      case tx @ vt.assets.UpdateAssetInfoTransaction(version, sender, assetId, name, description, timestamp, _, _, proofs, chainId) =>
        val (feeAsset, feeAmount) = tx.assetFee

        val data = UpdateAssetInfoTransactionData()
          .withAssetId(assetId.id.toByteString)
          .withName(name)
          .withDescription(description)

        PBTransactions.create(sender, chainId, feeAmount, feeAsset, timestamp, version, proofs, Data.UpdateAssetInfo(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  def toPBInvokeScriptData(dappAddress: AddressOrAlias, fcOpt: Option[FUNCTION_CALL], payment: Seq[Payment]): InvokeScriptTransactionData = {
    import com.wavesplatform.lang.v1.Serde

    InvokeScriptTransactionData(
      Some(PBRecipients.create(dappAddress)),
      ByteString.copyFrom(Deser.serializeOption(fcOpt)(Serde.serialize(_))),
      payment.map(p => (p.assetId, p.amount): Amount)
    )
  }

  def toVanillaDataEntry(de: DataTransactionData.DataEntry): com.wavesplatform.state.DataEntry[_] = {
    import DataTransactionData.DataEntry.{Value => DEV}

    de.value match {
      case DEV.IntValue(num)      => IntegerDataEntry(de.key, num)
      case DEV.BoolValue(bool)    => BooleanDataEntry(de.key, bool)
      case DEV.BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteStr)
      case DEV.StringValue(str)   => StringDataEntry(de.key, str)
      case DEV.Empty              => EmptyDataEntry(de.key)
    }
  }

  def toPBDataEntry(de: com.wavesplatform.state.DataEntry[_]): DataTransactionData.DataEntry = {
    DataTransactionData.DataEntry(
      de.key,
      de match {
        case IntegerDataEntry(_, value) => DataTransactionData.DataEntry.Value.IntValue(value)
        case BooleanDataEntry(_, value) => DataTransactionData.DataEntry.Value.BoolValue(value)
        case BinaryDataEntry(_, value)  => DataTransactionData.DataEntry.Value.BinaryValue(value.toByteString)
        case StringDataEntry(_, value)  => DataTransactionData.DataEntry.Value.StringValue(value)
        case EmptyDataEntry(_)          => DataTransactionData.DataEntry.Value.Empty
      }
    )
  }

  def toVanillaScript(script: ByteString): Option[com.wavesplatform.lang.script.Script] = {
    import com.wavesplatform.common.utils._
    if (script.isEmpty) None else Some(ScriptReader.fromBytes(script.toByteArray).explicitGet())
  }

  def toPBScript(script: Option[com.wavesplatform.lang.script.Script]): ByteString = script match {
    case Some(sc) => ByteString.copyFrom(sc.bytes().arr)
    case None     => ByteString.EMPTY
  }
}
