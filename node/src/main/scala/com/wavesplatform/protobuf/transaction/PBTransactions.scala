package com.wavesplatform.protobuf.transaction

import cats.syntax.traverse.*
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.protobuf.utils.PBImplicitConversions.*
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction as vt
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, NegativeAmount}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{EthereumTransaction, Proofs, TxDecimals, TxExchangeAmount, TxExchangePrice, TxNonNegativeAmount, TxPositiveAmount, TxValidationError}
import com.wavesplatform.utils.StringBytes
import scalapb.UnknownFieldSet.empty

import scala.util.Try

object PBTransactions {

  def createGenesis(chainId: Byte, timestamp: Long, signature: ByteStr, data: GenesisTransactionData): SignedTransaction =
    new SignedTransaction(
      SignedTransaction.Transaction.WavesTransaction(Transaction(chainId, timestamp = timestamp, data = Data.Genesis(data))),
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
      SignedTransaction.Transaction
        .WavesTransaction(Transaction(chainId, sender.toByteString, Some((feeAssetId, fee): Amount), timestamp, version, data)),
      proofsArray.map(bs => ByteString.copyFrom(bs.arr))
    )

  def vanillaUnsafe(signedTx: PBSignedTransaction): VanillaTransaction = {
    import com.wavesplatform.common.utils.*
    vanilla(signedTx, unsafe = true).explicitGet()
  }

  def tryToVanilla(signedTx: PBSignedTransaction): Try[VanillaTransaction] =
    Try(vanilla(signedTx, unsafe = false).explicitGet())

  def vanilla(signedTx: PBSignedTransaction, unsafe: Boolean): Either[ValidationError, VanillaTransaction] =
    signedTx.transaction match {
      case SignedTransaction.Transaction.Empty                      => Left(GenericError("Transaction must be specified"))
      case SignedTransaction.Transaction.EthereumTransaction(value) => EthereumTransaction(value.toByteArray)
      case SignedTransaction.Transaction.WavesTransaction(parsedTx) =>
        val (feeAsset, feeAmount) = PBAmounts.toAssetAndAmount(parsedTx.fee.getOrElse(Amount.defaultInstance))
        val sender = Option(parsedTx.senderPublicKey)
          .filterNot(_.isEmpty)
          .map(pk => PublicKey(pk.toByteArray))
          .orNull
        for {
          tx <- if (unsafe)
            Right(
              createVanillaUnsafe(
                parsedTx.version,
                parsedTx.chainId.toByte,
                sender,
                feeAmount,
                feeAsset,
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
                feeAmount,
                feeAsset,
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
      case Data.Genesis(GenesisTransactionData(recipient, amount, `empty`)) =>
        for {
          addr <- PBRecipients.toAddress(recipient.toByteArray, chainId)
          tx   <- vt.GenesisTransaction.create(addr, amount, timestamp)
        } yield tx

      case Data.Payment(PaymentTransactionData(recipient, amount, `empty`)) =>
        for {
          addr <- PBRecipients.toAddress(recipient.toByteArray, chainId)
          tx   <- vt.PaymentTransaction.create(sender, addr, amount, feeAmount, timestamp, signature)
        } yield tx

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment, `empty`)) =>
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

      case Data.CreateAlias(CreateAliasTransactionData(alias, _)) =>
        vt.CreateAliasTransaction.create(version.toByte, sender, alias, feeAmount, timestamp, proofs, chainId)

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script, `empty`)) =>
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

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount, `empty`)), reissuable, `empty`)) =>
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

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount, `empty`)), `empty`)) =>
        vt.assets.BurnTransaction.create(version.toByte, sender, IssuedAsset(assetId.toByteStr), amount, feeAmount, timestamp, proofs, chainId)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script, `empty`)) =>
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

      case Data.SetScript(SetScriptTransactionData(script, `empty`)) =>
        vt.smart.SetScriptTransaction.create(
          version.toByte,
          sender,
          toVanillaScript(script),
          feeAmount,
          timestamp,
          proofs,
          chainId
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount, `empty`)) =>
        for {
          address <- recipient.toAddressOrAlias(chainId)
          tx      <- vt.lease.LeaseTransaction.create(version.toByte, sender, address, amount, feeAmount, timestamp, proofs)
        } yield tx

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId, `empty`)) =>
        vt.lease.LeaseCancelTransaction.create(version.toByte, sender, leaseId.toByteStr, feeAmount, timestamp, proofs, chainId)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(order1, order2), `empty`)) =>
        for {
          order1 <- PBOrders.vanilla(order1)
          order2 <- PBOrders.vanilla(order2)
          tx <- vt.assets.exchange.ExchangeTransaction.create(
            version.toByte,
            order1,
            order2,
            amount,
            price,
            buyMatcherFee,
            sellMatcherFee,
            feeAmount,
            timestamp,
            proofs,
            chainId
          )
        } yield tx

      case Data.DataTransaction(dt) =>
        vt.DataTransaction.create(version.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs, chainId)

      case Data.MassTransfer(mt) =>
        for {
          parsedTransfers <- mt.transfers.flatMap { t =>
            t.getRecipient.toAddressOrAlias(chainId).toOption.map { addressOrAlias =>
              TxNonNegativeAmount(t.amount)(NegativeAmount(t.amount, "asset"))
                .map(ParsedTransfer(addressOrAlias, _))
            }
          }.sequence
          tx <- vt.transfer.MassTransferTransaction.create(
            version.toByte,
            sender,
            PBAmounts.toVanillaAssetId(mt.assetId),
            parsedTransfers,
            feeAmount,
            timestamp,
            mt.attachment.toByteStr,
            proofs,
            chainId
          )
        } yield tx

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee, `empty`)), `empty`)) =>
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

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments, `empty`)) =>
        import cats.instances.either.*
        import cats.instances.option.*
        import cats.syntax.traverse.*

        for {
          dApp <- PBRecipients.toAddressOrAlias(dappAddress, chainId)

          fcOpt <- Deser
            .parseOption(functionCall.asReadOnlyByteBuffer())(SerdeV1.deserializeFunctionCall)
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
            fcOpt,
            payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
            feeAmount,
            feeAssetId,
            timestamp,
            proofs,
            chainId
          )
        } yield tx

      case Data.UpdateAssetInfo(UpdateAssetInfoTransactionData(assetId, name, description, `empty`)) =>
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

      case Data.InvokeExpression(InvokeExpressionTransactionData(expressionBytes, `empty`)) =>
        for {
          expression <- toVanillaScript(expressionBytes) match {
            case Some(e: ExprScript) => Right(e)
            case Some(_)             => Left(GenericError("Unexpected expression type for InvokeExpression"))
            case None                => Left(GenericError(s"Unexpected empty expression bytes for InvokeExpression"))
          }
          tx <- InvokeExpressionTransaction.create(
            version.toByte,
            sender,
            expression,
            feeAmount,
            feeAssetId,
            timestamp,
            proofs,
            chainId
          )
        } yield tx

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
    import com.wavesplatform.common.utils.*

    val signature = proofs.toSignature
    data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount, `empty`)) =>
        vt.GenesisTransaction(
          PBRecipients.toAddress(recipient.toByteArray, chainId).explicitGet(),
          TxNonNegativeAmount.unsafeFrom(amount),
          timestamp,
          signature,
          chainId
        )

      case Data.Payment(PaymentTransactionData(recipient, amount, `empty`)) =>
        vt.PaymentTransaction(
          sender,
          PBRecipients.toAddress(recipient.toByteArray, chainId).explicitGet(),
          TxPositiveAmount.unsafeFrom(amount),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          signature,
          chainId
        )

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment, `empty`)) =>
        vt.transfer.TransferTransaction(
          version.toByte,
          sender,
          recipient.toAddressOrAlias(chainId).explicitGet(),
          amount.vanillaAssetId,
          TxPositiveAmount.unsafeFrom(amount.longAmount),
          feeAssetId,
          TxPositiveAmount.unsafeFrom(feeAmount),
          attachment.toByteStr,
          timestamp,
          proofs,
          chainId
        )

      case Data.CreateAlias(CreateAliasTransactionData(alias, `empty`)) =>
        vt.CreateAliasTransaction(
          version.toByte,
          sender,
          alias,
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          Proofs(signature),
          chainId
        )

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script, `empty`)) =>
        vt.assets.IssueTransaction(
          version.toByte,
          sender,
          name.toByteString,
          description.toByteString,
          TxPositiveAmount.unsafeFrom(quantity),
          TxDecimals.unsafeFrom(decimals.toByte),
          reissuable,
          toVanillaScript(script),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount, `empty`)), reissuable, `empty`)) =>
        vt.assets.ReissueTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          TxPositiveAmount.unsafeFrom(amount),
          reissuable,
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount, `empty`)), `empty`)) =>
        vt.assets.BurnTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          TxNonNegativeAmount.unsafeFrom(amount),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script, `empty`)) =>
        vt.assets.SetAssetScriptTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          toVanillaScript(script),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.SetScript(SetScriptTransactionData(script, `empty`)) =>
        vt.smart.SetScriptTransaction(
          version.toByte,
          sender,
          toVanillaScript(script),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount, `empty`)) =>
        vt.lease.LeaseTransaction(
          version.toByte,
          sender,
          recipient.toAddressOrAlias(chainId).explicitGet(),
          TxPositiveAmount.unsafeFrom(amount),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId, `empty`)) =>
        vt.lease.LeaseCancelTransaction(version.toByte, sender, leaseId.toByteStr, TxPositiveAmount.unsafeFrom(feeAmount), timestamp, proofs, chainId)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(buyOrder, sellOrder), `empty`)) =>
        vt.assets.exchange.ExchangeTransaction(
          version.toByte,
          PBOrders.vanilla(buyOrder).explicitGet(),
          PBOrders.vanilla(sellOrder).explicitGet(),
          TxExchangeAmount.unsafeFrom(amount),
          TxExchangePrice.unsafeFrom(price),
          buyMatcherFee,
          sellMatcherFee,
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction(
          version.toByte,
          sender,
          dt.data.toList.map(toVanillaDataEntry),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction(
          version.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers
            .flatMap(t => t.getRecipient.toAddressOrAlias(chainId).toOption.map(ParsedTransfer(_, TxNonNegativeAmount.unsafeFrom(t.amount))))
            .toList,
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          mt.attachment.toByteStr,
          proofs,
          chainId
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee, `empty`)), `empty`)) =>
        vt.assets.SponsorFeeTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          Some(minFee).filter(_ > 0).map(TxPositiveAmount.unsafeFrom),
          TxPositiveAmount.unsafeFrom(feeAmount),
          timestamp,
          proofs,
          chainId
        )

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments, `empty`)) =>
        vt.smart.InvokeScriptTransaction(
          version.toByte,
          sender,
          PBRecipients.toAddressOrAlias(dappAddress, chainId).explicitGet(),
          Deser
            .parseOption(functionCall.asReadOnlyByteBuffer())(SerdeV1.deserializeFunctionCall)
            .map(_.explicitGet()),
          payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
          TxPositiveAmount.unsafeFrom(feeAmount),
          feeAssetId,
          timestamp,
          proofs,
          chainId
        )

      case Data.UpdateAssetInfo(UpdateAssetInfoTransactionData(assetId, name, description, `empty`)) =>
        vt.assets.UpdateAssetInfoTransaction(
          version.toByte,
          sender,
          IssuedAsset(assetId.toByteStr),
          name,
          description,
          timestamp,
          TxPositiveAmount.unsafeFrom(feeAmount),
          feeAssetId,
          proofs,
          chainId
        )

      case Data.InvokeExpression(InvokeExpressionTransactionData(expressionBytes, `empty`)) =>
        InvokeExpressionTransaction(
          version.toByte,
          sender,
          toVanillaScript(expressionBytes).get.asInstanceOf[ExprScript],
          TxPositiveAmount.unsafeFrom(feeAmount),
          feeAssetId,
          timestamp,
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
          GenesisTransactionData(PBRecipients.create(recipient).getPublicKeyHash, amount.value)
        )

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature, chainId) =>
        val data = PaymentTransactionData(PBRecipients.create(recipient).getPublicKeyHash, amount.value)
        PBTransactions.create(sender, chainId, fee.value, Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case tx: vt.transfer.TransferTransaction =>
        import tx.*
        val data = TransferTransactionData(Some(recipient.toPB), Some((assetId, amount.value)), attachment.toByteString)
        PBTransactions.create(sender, chainId, fee.value, feeAssetId, timestamp, version, proofs, Data.Transfer(data))

      case tx: vt.CreateAliasTransaction =>
        import tx.*
        val data = CreateAliasTransactionData(alias.name)
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.CreateAlias(data))

      case tx: vt.assets.exchange.ExchangeTransaction =>
        import tx.*
        val data = ExchangeTransactionData(
          amount.value,
          price.value,
          buyMatcherFee,
          sellMatcherFee,
          Seq(PBOrders.protobuf(order1), PBOrders.protobuf(order2))
        )
        PBTransactions.create(tx.sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.Exchange(data))

      case tx: vt.assets.IssueTransaction =>
        import tx.*
        val data = IssueTransactionData(name.toStringUtf8, description.toStringUtf8, quantity.value, decimals.value, reissuable, toPBScript(script))
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.Issue(data))

      case tx: vt.assets.ReissueTransaction =>
        import tx.*
        val data = ReissueTransactionData(Some(Amount(asset.id.toByteString, quantity.value)), reissuable)
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.Reissue(data))

      case tx: vt.assets.BurnTransaction =>
        import tx.*
        val data = BurnTransactionData(Some(Amount(asset.id.toByteString, quantity.value)))
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.Burn(data))

      case tx @ vt.assets.SetAssetScriptTransaction(_, sender, assetId, script, fee, timestamp, proofs, chainId) =>
        val data = SetAssetScriptTransactionData(assetId.id.toByteString, toPBScript(script))
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, tx.version, proofs, Data.SetAssetScript(data))

      case tx @ vt.smart.SetScriptTransaction(_, sender, script, fee, timestamp, proofs, chainId) =>
        val data = SetScriptTransactionData(toPBScript(script))
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, tx.version, proofs, Data.SetScript(data))

      case tx: vt.lease.LeaseTransaction =>
        import tx.*
        val data = LeaseTransactionData(Some(recipient.toPB), amount.value)
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.Lease(data))

      case tx: vt.lease.LeaseCancelTransaction =>
        import tx.*
        val data = LeaseCancelTransactionData(leaseId.toByteString)
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(version, sender, assetId, transfers, fee, timestamp, attachment, proofs, chainId) =>
        val data = MassTransferTransactionData(
          PBAmounts.toPBAssetId(assetId),
          transfers.map(pt => MassTransferTransactionData.Transfer(Some(pt.address.toPB), pt.amount.value)),
          attachment.toByteString
        )
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.MassTransfer(data))

      case tx @ vt.DataTransaction(version, sender, data, fee, timestamp, proofs, chainId) =>
        val txData = DataTransactionData(data.map(toPBDataEntry))
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(version, sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs, chainId) =>
        val data = SponsorFeeTransactionData(Some(Amount(assetId.id.toByteString, minSponsoredAssetFee.map(_.value).getOrElse(0L))))
        PBTransactions.create(sender, chainId, fee.value, tx.feeAssetId, timestamp, version, proofs, Data.SponsorFee(data))

      case vt.smart.InvokeScriptTransaction(version, sender, dappAddress, fcOpt, payment, fee, feeAssetId, timestamp, proofs, chainId) =>
        val data = Data.InvokeScript(toPBInvokeScriptData(dappAddress, fcOpt, payment))
        PBTransactions.create(sender, chainId, fee.value, feeAssetId, timestamp, version, proofs, data)

      case vt.assets.UpdateAssetInfoTransaction(version, sender, assetId, name, description, timestamp, fee, feeAssetId, proofs, chainId) =>
        val data = UpdateAssetInfoTransactionData()
          .withAssetId(assetId.id.toByteString)
          .withName(name)
          .withDescription(description)

        PBTransactions.create(sender, chainId, fee.value, feeAssetId, timestamp, version, proofs, Data.UpdateAssetInfo(data))

      case tx @ InvokeExpressionTransaction(version, sender, _, fee, feeAssetId, timestamp, proofs, chainId) =>
        val data = Data.InvokeExpression(InvokeExpressionTransactionData(tx.expressionBytes.toByteString))
        PBTransactions.create(sender, chainId, fee.value, feeAssetId, timestamp, version, proofs, data)

      case et: EthereumTransaction =>
        PBSignedTransaction(PBSignedTransaction.Transaction.EthereumTransaction(ByteString.copyFrom(et.bytes())))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  def toPBInvokeScriptData(dAppAddress: AddressOrAlias, exprOpt: Option[EXPR], payment: Seq[Payment]): InvokeScriptTransactionData = {
    InvokeScriptTransactionData(
      Some(PBRecipients.create(dAppAddress)),
      ByteString.copyFrom(Deser.serializeOption(exprOpt)(SerdeV1.serialize(_))),
      payment.map(p => (p.assetId, p.amount): Amount)
    )
  }

  def toVanillaDataEntry(de: DataTransactionData.DataEntry): com.wavesplatform.state.DataEntry[?] = {
    import DataTransactionData.DataEntry.Value as DEV

    de.value match {
      case DEV.IntValue(num)      => IntegerDataEntry(de.key, num)
      case DEV.BoolValue(bool)    => BooleanDataEntry(de.key, bool)
      case DEV.BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteStr)
      case DEV.StringValue(str)   => StringDataEntry(de.key, str)
      case DEV.Empty              => EmptyDataEntry(de.key)
    }
  }

  def toPBDataEntry(de: com.wavesplatform.state.DataEntry[?]): DataTransactionData.DataEntry = {
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
    import com.wavesplatform.common.utils.*
    if (script.isEmpty) None else Some(ScriptReader.fromBytes(script.toByteArray).explicitGet())
  }

  def toPBScript(script: Option[com.wavesplatform.lang.script.Script]): ByteString = script match {
    case Some(sc) => ByteString.copyFrom(sc.bytes().arr)
    case None     => ByteString.EMPTY
  }

  // Stores Ethereum txs as-is
  def toByteArrayMerkle(tx: VanillaTransaction): Array[Byte] = tx match {
    case et: EthereumTransaction => et.bytes()
    case vt                      => toByteArray(vt)
  }

  def toByteArray(tx: VanillaTransaction): Array[Byte] =
    PBTransactionSerializer.bytes(tx)
}
