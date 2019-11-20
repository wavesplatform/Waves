package com.wavesplatform.protobuf.transaction
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.protobuf.transaction.{Script => PBScript}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Proofs, TxValidationError}
import com.wavesplatform.{transaction => vt}

object PBTransactions {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def create(
      sender: com.wavesplatform.account.PublicKey = PublicKey.empty,
      chainId: Byte = 0,
      fee: Long = 0L,
      feeAssetId: VanillaAssetId = Waves,
      timestamp: Long = 0L,
      version: Int = 0,
      proofsArray: Seq[com.wavesplatform.common.state.ByteStr] = Nil,
      data: com.wavesplatform.protobuf.transaction.Transaction.Data = com.wavesplatform.protobuf.transaction.Transaction.Data.Empty
  ): SignedTransaction = {
    new SignedTransaction(
      Some(Transaction(AddressScheme.current.chainId, sender: ByteStr, Some((feeAssetId, fee): Amount), timestamp, version, data)),
      proofsArray.map(bs => ByteString.copyFrom(bs.arr))
    )
  }

  def vanillaUnsafe(signedTx: PBSignedTransaction): VanillaTransaction = {
    import com.wavesplatform.common.utils._
    vanilla(signedTx, unsafe = true).explicitGet()
  }

  def vanilla(signedTx: PBSignedTransaction, unsafe: Boolean = false): Either[ValidationError, VanillaTransaction] = {
    for {
      parsedTx <- signedTx.transaction.toRight(GenericError("Transaction must be specified"))
      fee      <- parsedTx.fee.toRight(GenericError("Fee must be specified"))
      _        <- Either.cond(parsedTx.data.isDefined, (), GenericError("Transaction data must be specified"))
      feeAmount = PBAmounts.toAssetAndAmount(fee)
      sender    = PublicKey(parsedTx.senderPublicKey.toByteArray)
      tx <- if (unsafe)
        Right(
          createVanillaUnsafe(
            parsedTx.version,
            parsedTx.chainId.toByte,
            sender,
            feeAmount._2,
            feeAmount._1,
            parsedTx.timestamp,
            Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
            parsedTx.data
          )
        )
      else
        createVanilla(
          parsedTx.version,
          parsedTx.chainId.toByte,
          sender,
          feeAmount._2,
          feeAmount._1,
          parsedTx.timestamp,
          Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
          parsedTx.data
        )
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
          addr <- PBRecipients.toAddress(recipient)
          tx   <- vt.GenesisTransaction.create(addr, amount, timestamp)
        } yield tx

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        for {
          addr <- PBRecipients.toAddress(recipient)
          tx   <- vt.PaymentTransaction.create(sender, Address.fromBytes(recipient.toByteArray).right.get, amount, feeAmount, timestamp, signature)
        } yield tx

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        for {
          address <- recipient.toAddressOrAlias
        } yield vt.transfer.TransferTransaction(
          version.toByte,
          sender,
          address,
          amount.vanillaAssetId,
          amount.longAmount,
          feeAssetId,
          feeAmount,
          attachment.toByteArray,
          timestamp,
          proofs
        )

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        for {
          alias <- com.wavesplatform.account.Alias.createWithChainId(alias, chainId)
          tx    <- vt.CreateAliasTransaction.create(version.toByte, sender, alias, feeAmount, timestamp, Proofs(Seq(signature)))
        } yield tx

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        vt.assets.IssueTransaction.create(
          version.toByte,
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

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount)), reissuable)) =>
        vt.assets.ReissueTransaction.create(version.toByte, sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, proofs)

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount)))) =>
        vt.assets.BurnTransaction.create(version.toByte, sender, IssuedAsset(assetId), amount, feeAmount, timestamp, proofs)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction.create(
          1.toByte,
          sender,
          IssuedAsset(assetId),
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction.create(
          1.toByte,
          sender,
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        for {
          address <- recipient.toAddressOrAlias
          tx      <- vt.lease.LeaseTransaction.create(version.toByte, sender, address, amount, feeAmount, timestamp, proofs)
        } yield tx

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        vt.lease.LeaseCancelTransaction.create(version.toByte, sender, leaseId.toByteArray, feeAmount, timestamp, proofs)

      case Data.Exchange(ExchangeTransactionData(amount, price, buyMatcherFee, sellMatcherFee, Seq(buyOrder, sellOrder))) =>
        vt.assets.exchange.ExchangeTransaction.create(
          2.toByte,
          PBOrders.vanilla(buyOrder),
          PBOrders.vanilla(sellOrder),
          amount,
          price,
          buyMatcherFee,
          sellMatcherFee,
          feeAmount,
          timestamp,
          proofs
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction.create(1.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs)

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction.create(
          1.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers.flatMap(t => t.getAddress.toAddressOrAlias.toOption.map(ParsedTransfer(_, t.amount))).toList,
          feeAmount,
          timestamp,
          mt.attachment.toByteArray,
          proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction.create(1.toByte, sender, IssuedAsset(assetId), Option(minFee).filter(_ > 0), feeAmount, timestamp, proofs)

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments)) =>
        import com.wavesplatform.common.utils._
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL

        for {
          dApp <- PBRecipients.toAddressOrAlias(dappAddress)

          desFCOpt = Deser.parseOption(functionCall.toByteArray, 0)(Serde.deserialize(_))._1

          _ <- Either.cond(
            desFCOpt.isEmpty || desFCOpt.get.isRight,
            (),
            GenericError(s"Invalid InvokeScript function call: ${desFCOpt.get.left.get}")
          )

          fcOpt = desFCOpt.map(_.explicitGet()._1)

          _ <- Either.cond(fcOpt.isEmpty || fcOpt.exists(_.isInstanceOf[FUNCTION_CALL]), (), GenericError(s"Not a function call: $fcOpt"))

          tx <- vt.smart.InvokeScriptTransaction.create(
            1.toByte,
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
        vt.GenesisTransaction(PBRecipients.toAddress(recipient).explicitGet(), amount, timestamp, signature)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction(sender, PBRecipients.toAddress(recipient).explicitGet(), amount, feeAmount, timestamp, signature)

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        vt.transfer.TransferTransaction(
          version.toByte,
          sender,
          recipient.toAddressOrAlias.explicitGet(),
          amount.vanillaAssetId,
          amount.longAmount,
          feeAssetId,
          feeAmount,
          attachment.toByteArray,
          timestamp,
          proofs
        )

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        vt.CreateAliasTransaction(
          version.toByte,
          sender,
          com.wavesplatform.account.Alias.createWithChainId(alias, chainId).explicitGet(),
          feeAmount,
          timestamp,
          Proofs(signature)
        )

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        vt.assets.IssueTransaction(
          version.toByte,
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

      case Data.Reissue(ReissueTransactionData(Some(Amount(assetId, amount)), reissuable)) =>
        vt.assets.ReissueTransaction(version.toByte, sender, IssuedAsset(assetId), amount, reissuable, feeAmount, timestamp, proofs)

      case Data.Burn(BurnTransactionData(Some(Amount(assetId, amount)))) =>
        vt.assets.BurnTransaction(version.toByte, sender, IssuedAsset(assetId), amount, feeAmount, timestamp, proofs)

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(
          1.toByte,
          sender,
          IssuedAsset(assetId),
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(
          1.toByte,
          sender,
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          feeAmount,
          timestamp,
          proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        vt.lease.LeaseTransaction(version.toByte, sender, recipient.toAddressOrAlias.explicitGet(), amount, feeAmount, timestamp, proofs)

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        vt.lease.LeaseCancelTransaction(version.toByte, sender, leaseId.toByteArray, feeAmount, timestamp, proofs)

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
          proofs
        )

      case Data.DataTransaction(dt) =>
        vt.DataTransaction(1.toByte, sender, dt.data.toList.map(toVanillaDataEntry), feeAmount, timestamp, proofs)

      case Data.MassTransfer(mt) =>
        vt.transfer.MassTransferTransaction(
          1.toByte,
          sender,
          PBAmounts.toVanillaAssetId(mt.assetId),
          mt.transfers.flatMap(t => t.getAddress.toAddressOrAlias.toOption.map(ParsedTransfer(_, t.amount))).toList,
          feeAmount,
          timestamp,
          mt.attachment.toByteArray,
          proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(Amount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction(1.toByte, sender, IssuedAsset(assetId), Option(minFee).filter(_ > 0), feeAmount, timestamp, proofs)

      case Data.InvokeScript(InvokeScriptTransactionData(Some(dappAddress), functionCall, payments)) =>
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL

        vt.smart.InvokeScriptTransaction(
          1.toByte,
          sender,
          PBRecipients.toAddressOrAlias(dappAddress).explicitGet(),
          Deser
            .parseByteArrayOptionWithLength(functionCall.asReadOnlyByteBuffer())
            .map(Serde.deserialize(_, all = false))
            .map(_.explicitGet()._1.asInstanceOf[FUNCTION_CALL]),
          payments.map(p => vt.smart.InvokeScriptTransaction.Payment(p.longAmount, PBAmounts.toVanillaAssetId(p.assetId))),
          feeAmount,
          feeAssetId,
          timestamp,
          proofs
        )

      case other =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $other")
    }
  }

  def protobuf(tx: VanillaTransaction): PBSignedTransaction = {
    val chainId = tx.chainByte.getOrElse(AddressScheme.current.chainId)

    tx match {
      case vt.GenesisTransaction(recipient, amount, timestamp, signature) =>
        val data = GenesisTransactionData(PBRecipients.create(recipient).getAddress, amount)
        PBTransactions.create(
          sender = PublicKey(Array.emptyByteArray),
          chainId = 0: Byte,
          timestamp = timestamp,
          version = 1,
          proofsArray = Seq(signature),
          data = Data.Genesis(data)
        )

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature) =>
        val data = PaymentTransactionData(PBRecipients.create(recipient).getAddress, amount)
        PBTransactions.create(sender, chainId, fee, Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case tx: vt.transfer.TransferTransaction =>
        import tx._
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), ByteString.copyFrom(attachment))
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
          Seq(PBOrders.protobuf(buyOrder), PBOrders.protobuf(sellOrder))
        )
        PBTransactions.create(tx.sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Exchange(data))

      case tx: vt.assets.IssueTransaction =>
        import tx._
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, script.map(s => PBScript(s.bytes())))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Issue(data))

      case tx: vt.assets.ReissueTransaction =>
        import tx._
        val data = ReissueTransactionData(Some(Amount(asset.id, quantity)), reissuable)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Reissue(data))

      case tx: vt.assets.BurnTransaction =>
        import tx._
        val data = BurnTransactionData(Some(Amount(asset.id, quantity)))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Burn(data))

      case tx @ vt.assets.SetAssetScriptTransaction(_, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(assetId.id, script.map(s => PBScript(s.bytes())))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, tx.version, proofs, Data.SetAssetScript(data))

      case tx @ vt.smart.SetScriptTransaction(_, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script.map(s => PBScript(s.bytes())))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, tx.version, proofs, Data.SetScript(data))

      case tx: vt.lease.LeaseTransaction =>
        import tx._
        val data = LeaseTransactionData(Some(recipient), amount)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.Lease(data))

      case tx: vt.lease.LeaseCancelTransaction =>
        import tx._
        val data = LeaseCancelTransactionData(leaseId)
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(version, sender, assetId, transfers, fee, timestamp, attachment, proofs) =>
        val data = MassTransferTransactionData(
          PBAmounts.toPBAssetId(assetId),
          transfers.map(pt => MassTransferTransactionData.Transfer(Some(pt.address), pt.amount)),
          attachment: ByteStr
        )
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.MassTransfer(data))

      case tx @ vt.DataTransaction(version, sender, data, fee, timestamp, proofs) =>
        val txData = DataTransactionData(data.map(toPBDataEntry))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(version, sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(Some(Amount(assetId.id, minSponsoredAssetFee.getOrElse(0L))))
        PBTransactions.create(sender, chainId, fee, tx.assetFee._1, timestamp, version, proofs, Data.SponsorFee(data))

      case vt.smart.InvokeScriptTransaction(_, sender, dappAddress, fcOpt, payment, fee, feeAssetId, timestamp, proofs) =>
        import com.wavesplatform.lang.v1.Serde
        import com.wavesplatform.serialization.Deser

        val data = InvokeScriptTransactionData(
          Some(PBRecipients.create(dappAddress)),
          ByteString.copyFrom(Deser.serializeOptionOfArrayWithLength(fcOpt)(Serde.serialize(_))),
          payment.map(p => (p.assetId, p.amount): Amount)
        )
        PBTransactions.create(sender, chainId, fee, feeAssetId, timestamp, 1, proofs, Data.InvokeScript(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  def toVanillaDataEntry(de: DataTransactionData.DataEntry): com.wavesplatform.state.DataEntry[_] = {
    import DataTransactionData.DataEntry.Value._

    de.value match {
      case IntValue(num)      => IntegerDataEntry(de.key, num)
      case BoolValue(bool)    => BooleanDataEntry(de.key, bool)
      case BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteArray)
      case StringValue(str)   => StringDataEntry(de.key, str)
      case Empty              => throw new IllegalArgumentException(s"Empty entries not supported: $de")
    }
  }

  def toPBDataEntry(de: com.wavesplatform.state.DataEntry[_]): DataTransactionData.DataEntry = {
    DataTransactionData.DataEntry(
      de.key,
      de match {
        case IntegerDataEntry(_, value) => DataTransactionData.DataEntry.Value.IntValue(value)
        case BooleanDataEntry(_, value) => DataTransactionData.DataEntry.Value.BoolValue(value)
        case BinaryDataEntry(_, value)  => DataTransactionData.DataEntry.Value.BinaryValue(value)
        case StringDataEntry(_, value)  => DataTransactionData.DataEntry.Value.StringValue(value)
      }
    )
  }
}
