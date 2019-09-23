package com.wavesplatform

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.testing.{ScriptGen, TypedScriptGen}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.settings.{Constants, FunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer}
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen.{alphaLowerChar, alphaUpperChar, frequency, numChar}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import scala.concurrent.duration._
import scala.util.Random

trait TransactionGen extends TransactionGenBase { _: Suite =>

}

trait TransactionGenBase extends ScriptGen with TypedScriptGen with NTPTime { _: Suite =>

  val ScriptExtraFee                  = 400000L
  protected def waves(n: Float): Long = (n * 100000000L).toLong

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val bytes64gen: Gen[Array[Byte]] = byteArrayGen(64)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    for {
      length <- Gen.chooseNum(minSize, maxSize)
      bytes  <- byteArrayGen(length)
    } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz =>
      Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray)
    }
  }

  val accountGen: Gen[KeyPair] = bytes32gen.map(seed => KeyPair(seed))

  val aliasSymbolChar: Gen[Char] = Gen.oneOf('.', '@', '_', '-')

  val invalidAliasSymbolChar: Gen[Char] = Gen.oneOf('~', '`', '!', '#', '$', '%', '^', '&', '*', '=', '+')

  val aliasAlphabetGen: Gen[Char] = frequency((1, numChar), (1, aliasSymbolChar), (9, alphaLowerChar))

  val invalidAliasAlphabetGen: Gen[Char] = frequency((1, numChar), (3, invalidAliasSymbolChar), (9, alphaUpperChar))

  val validAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, aliasAlphabetGen)
  } yield aliasChars.mkString

  val aliasGen: Gen[Alias] = for {
    str <- validAliasStringGen
  } yield Alias.create(str.mkString).explicitGet()

  val funcNameGen: Gen[String] = for {
    length        <- Gen.chooseNum(1, ContractLimits.MaxAnnotatedFunctionNameInBytes)
    funcNameChars <- Gen.listOfN(length, alphaLowerChar)
  } yield funcNameChars.mkString

  val invalidAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, invalidAliasAlphabetGen)
  } yield aliasChars.mkString

  val accountOrAliasGen: Gen[AddressOrAlias] = Gen.oneOf(aliasGen, accountGen.map(_.toAddress))

  def otherAccountGen(candidate: KeyPair): Gen[KeyPair] = accountGen.flatMap(Gen.oneOf(candidate, _))

  val positiveLongGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L / 100)
  val positiveIntGen: Gen[Int]   = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long]     = Gen.choose(400000, 100000000)

  val maxOrderTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + ntpTime.correctedTime())
  val timestampGen: Gen[Long]    = Gen.choose(1L, Long.MaxValue - 100)
  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(ntpTime.correctedTime() - _)

  def validTimestampGen(blockTimestamp: Long, back: FiniteDuration = 120.minutes, forward: FiniteDuration = 90.minutes): Gen[Long] =
    Gen.choose(blockTimestamp - back.toMillis, blockTimestamp + forward.toMillis)

  def validTimestampGen(blockTimestamp: Long, fs: FunctionalitySettings): Gen[Long] =
    validTimestampGen(blockTimestamp, fs.maxTransactionTimeBackOffset, fs.maxTransactionTimeForwardOffset)

  val wavesAssetGen: Gen[Option[ByteStr]] = Gen.const(None)
  val assetIdGen: Gen[Option[ByteStr]]    = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen.map(ByteStr(_)))))

  val assetPairGen = assetIdGen.flatMap {
    case None => bytes32gen.map(b => AssetPair(Waves, IssuedAsset(ByteStr(b))))
    case Some(a1bytes) =>
      val a2bytesGen: Gen[Option[Array[Byte]]] = byteArrayGen(31).map(a2bytes => Option(a1bytes.arr(0) +: a2bytes))

      Gen.oneOf(Gen.const(None), a2bytesGen).map { a2 =>
        val asset1 = IssuedAsset(a1bytes)
        val asset2 = a2.fold[Asset](Waves)(arr => IssuedAsset(ByteStr(arr)))
        AssetPair(asset1, asset2)
      }
  }

  val MinIssueFee = 100000000

  val issueParamGen = for {
    sender      <- accountGen
    assetName   <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
    decimals    <- Gen.choose(0: Byte, 8: Byte)
    reissuable  <- Arbitrary.arbitrary[Boolean]
    fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp   <- positiveLongGen
  } yield (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp)

  val proofsGen: Gen[Proofs] = for {
    proofsAmount <- Gen.choose(1, 8)
    proofs       <- Gen.listOfN(proofsAmount, genBoundedBytes(0, 50))
  } yield Proofs.create(proofs.map(ByteStr(_))).explicitGet()

  val scriptGen: Gen[Script]         = exprGen.map(e => ExprScript(e).explicitGet())
  val contractScriptGen: Gen[Script] = contractGen.map(e => ContractScript(V3, e).explicitGet())
  val contractOrExpr                 = Gen.oneOf(scriptGen, contractScriptGen)
  val setAssetScriptTransactionGen: Gen[(Seq[Transaction], SetAssetScriptTransaction)] = for {
    (sender, assetName, description, quantity, decimals, _, iFee, _) <- issueParamGen
    timestamp                                                        <- timestampGen
    proofs                                                           <- proofsGen
    script                                                           <- Gen.option(scriptGen)
    issue = IssueTransactionV2
      .selfSigned(AddressScheme.current.chainId, sender, assetName, description, quantity, decimals, reissuable = true, script, iFee, timestamp)
      .explicitGet()
  } yield (
    Seq(issue),
    SetAssetScriptTransaction
      .create(AddressScheme.current.chainId, sender, IssuedAsset(issue.id()), script, 1 * Constants.UnitsInWave + ScriptExtraFee, timestamp, proofs)
      .explicitGet()
  )

  val setScriptTransactionGen: Gen[SetScriptTransaction] = for {
    sender    <- accountGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
    proofs    <- proofsGen
    script    <- Gen.option(contractOrExpr)
  } yield SetScriptTransaction.create(sender, script, fee, timestamp, proofs).explicitGet()

  def selfSignedSetScriptTransactionGenP(sender: KeyPair, s: Script): Gen[SetScriptTransaction] =
    for {
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield SetScriptTransaction.selfSigned(sender, Some(s), fee, timestamp).explicitGet()

  val paymentGen: Gen[PaymentTransaction] = for {
    sender    <- accountGen
    recipient <- accountGen
    tx        <- paymentGeneratorP(sender, recipient)
  } yield tx

  val selfPaymentGen: Gen[PaymentTransaction] = accountGen.flatMap(acc => paymentGeneratorP(acc, acc))

  def paymentGeneratorP(sender: KeyPair, recipient: Address): Gen[PaymentTransaction] =
    timestampGen.flatMap(ts => paymentGeneratorP(ts, sender, recipient))

  def paymentGeneratorP(timestamp: Long, sender: KeyPair, recipient: Address): Gen[PaymentTransaction] =
    for {
      amount: Long <- positiveLongGen
      fee: Long    <- smallFeeGen
    } yield PaymentTransaction.create(sender, recipient, amount, fee, timestamp).explicitGet()

  private val leaseParamGen = for {
    sender    <- accountGen
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
    recipient <- accountGen
  } yield (sender, amount, fee, timestamp, recipient)

  def createLease(sender: KeyPair, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias): Gen[LeaseTransaction] = {
    val v1 = LeaseTransactionV1.selfSigned(sender, amount, fee, timestamp, recipient).explicitGet()
    val v2 = LeaseTransactionV2.selfSigned(sender, amount, fee, timestamp, recipient).explicitGet()
    Gen.oneOf(v1, v2)
  }

  def createLeaseCancel(sender: KeyPair, leaseId: ByteStr, cancelFee: Long, timestamp: Long): Gen[LeaseCancelTransaction] = {
    val v1 = LeaseCancelTransactionV1.selfSigned(sender, leaseId, cancelFee, timestamp + 1).explicitGet()
    val v2 = LeaseCancelTransactionV2
      .selfSigned(AddressScheme.current.chainId, sender, leaseId, cancelFee, timestamp + 1)
      .right
      .get
    Gen.oneOf(v1, v2)
  }
  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient)
    cancelFee                                   <- smallFeeGen
    leaseCancel                                 <- createLeaseCancel(sender, lease.id(), cancelFee, timestamp + 1)
  } yield (lease, leaseCancel)

  def leaseAndCancelGeneratorP(leaseSender: KeyPair, recipient: AddressOrAlias, timestamp: Long): Gen[(LeaseTransaction, LeaseCancelTransaction)] =
    for {
      (_, amount, fee, _, _) <- leaseParamGen
      lease                          <- createLease(leaseSender, amount, fee, timestamp, recipient)
      fee2                           <- smallFeeGen
      unlease                        <- createLeaseCancel(leaseSender, lease.id(), fee2, timestamp + 1)
    } yield (lease, unlease)

  def leaseAndCancelGeneratorP(leaseSender: KeyPair, recipient: AddressOrAlias): Gen[(LeaseTransaction, LeaseCancelTransaction)] =
    for {
      (_, amount, fee, timestamp, _) <- leaseParamGen
      lease                          <- createLease(leaseSender, amount, fee, timestamp, recipient)
      fee2                           <- smallFeeGen
      unlease                        <- createLeaseCancel(leaseSender, lease.id(), fee2, timestamp + 1)
    } yield (lease, unlease)

  val twoLeasesGen: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    amount2                                     <- positiveLongGen
    recipient2                                  <- accountGen
    fee2                                        <- smallFeeGen
    lease1                                      <- createLease(sender, amount, fee, timestamp, recipient)
    lease2                                      <- createLease(sender, amount2, fee2, timestamp + 1, recipient2)
  } yield (lease1, lease2)

  val leaseAndCancelWithOtherSenderGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    otherSender                                 <- accountGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient)
    fee2                                        <- smallFeeGen
    timestamp2                                  <- positiveLongGen
    leaseCancel                                 <- createLeaseCancel(otherSender, lease.id(), fee2, timestamp2)
  } yield (lease, leaseCancel)

  val leaseGen: Gen[LeaseTransaction]             = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[LeaseCancelTransaction] = leaseAndCancelGen.map(_._2)

  val transferParamGen = for {
    amount     <- positiveLongGen
    feeAmount  <- smallFeeGen
    assetId    <- Gen.option(bytes32gen)
    feeAssetId <- Gen.option(bytes32gen)
    timestamp  <- timestampGen
    sender     <- accountGen
    attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient  <- accountOrAliasGen
  } yield (
    Asset.fromCompatId(assetId.map(ByteStr(_))),
    sender,
    recipient,
    amount,
    timestamp,
    Asset.fromCompatId(feeAssetId.map(ByteStr(_))),
    feeAmount,
    attachment
  )

  def transferGeneratorP(sender: KeyPair, recipient: AddressOrAlias, assetId: Asset, feeAssetId: Asset): Gen[TransferTransactionV1] =
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet()

  def versionedTransferGeneratorP(sender: KeyPair, recipient: AddressOrAlias, assetId: Asset, feeAssetId: Asset): Gen[TransferTransactionV2] =
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV2
      .selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment)
      .explicitGet()

  def transferGeneratorP(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransactionV1] =
    for {
      amount                                    <- Gen.choose(1, maxAmount)
      (_, _, _, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, timestamp, Waves, feeAmount, attachment).explicitGet()

  def transferGeneratorPV2(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransactionV2] =
    for {
      amount                                    <- Gen.choose(1, maxAmount)
      (_, _, _, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV2.selfSigned(Waves, sender, recipient, amount, timestamp, Waves, feeAmount, attachment).explicitGet()

  def transferGeneratorP(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias, assetId: Asset, feeAssetId: Asset): Gen[TransferTransactionV1] =
    for {
      (_, _, _, amount, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet()

  def wavesTransferGeneratorP(sender: KeyPair, recipient: AddressOrAlias): Gen[TransferTransactionV1] =
    transferGeneratorP(sender, recipient, Waves, Waves)

  def wavesTransferGeneratorP(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias): Gen[TransferTransactionV1] =
    transferGeneratorP(timestamp, sender, recipient, Waves, Waves)

  def massTransferGeneratorP(sender: KeyPair, transfers: List[ParsedTransfer], assetId: Asset): Gen[MassTransferTransaction] =
    for {
      (_, _, _, _, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield MassTransferTransaction.selfSigned(assetId, sender, transfers, timestamp, feeAmount, attachment).explicitGet()

  def createWavesTransfer(
      sender: KeyPair,
      recipient: Address,
      amount: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransferTransactionV1] =
    TransferTransactionV1.selfSigned(Waves, sender, recipient, amount, timestamp, Waves, fee, Array())

  val transferV1Gen = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet())
    .label("transferTransaction")

  val transferV2Gen = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
    proofs                                                                             <- proofsGen
  } yield TransferTransactionV2
    .create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, proofs)
    .explicitGet())
    .label("VersionedTransferTransaction")

  def versionedTransferGenP(sender: PublicKey, recipient: Address, proofs: Proofs): Gen[TransferTransactionV2] =
    (for {
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TransferTransactionV2.create(Waves, sender, recipient, amt, timestamp, Waves, fee, Array.emptyByteArray, proofs).explicitGet())
      .label("VersionedTransferTransactionP")

  val transferWithWavesFeeGen = for {
    (assetId, sender, recipient, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, Waves, feeAmount, attachment).explicitGet()

  val selfTransferWithWavesFeeGen: Gen[TransferTransactionV1] = for {
    (assetId, sender, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, sender, amount, timestamp, Waves, feeAmount, attachment).explicitGet()

  val selfTransferGen: Gen[TransferTransactionV1] = for {
    (assetId, sender, _, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, sender, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet()

  val massTransferGen: Gen[MassTransferTransaction] = massTransferGen(MaxTransferCount)

  def massTransferGen(maxTransfersCount: Int): Gen[MassTransferTransaction] = {
    for {
      (assetId, sender, _, _, timestamp, _, feeAmount, attachment) <- transferParamGen
      transferCount                                                <- Gen.choose(0, maxTransfersCount)
      transferGen = for {
        recipient <- accountOrAliasGen
        amount    <- Gen.choose(1L, Long.MaxValue / maxTransfersCount)
      } yield ParsedTransfer(recipient, amount)
      recipients <- Gen.listOfN(transferCount, transferGen)
    } yield MassTransferTransaction.selfSigned(assetId, sender, recipients, timestamp, feeAmount, attachment).explicitGet()
  }

  val createAliasGen: Gen[CreateAliasTransaction] = for {
    timestamp: Long <- positiveLongGen
    sender          <- accountGen
    alias: Alias    <- aliasGen
    tx <- Gen.oneOf(
      CreateAliasTransactionV1.selfSigned(sender, alias, MinIssueFee, timestamp).explicitGet(),
      CreateAliasTransactionV2.selfSigned(sender, alias, MinIssueFee, timestamp).explicitGet()
    )
  } yield tx

  def createAliasGen(sender: KeyPair, alias: Alias, fee: Long, timestamp: Long): Gen[CreateAliasTransaction] = {
    for {
      tx <- Gen.oneOf(
        CreateAliasTransactionV1.selfSigned(sender, alias, fee, timestamp).explicitGet(),
        CreateAliasTransactionV2.selfSigned(sender, alias, fee, timestamp).explicitGet()
      )
    } yield tx
  }

  val issueReissueBurnGen: Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    amount <- positiveLongGen
    sender <- accountGen
    r      <- issueReissueBurnGeneratorP(amount, amount, amount, sender)
  } yield r

  def issueReissueBurnGeneratorP(issueQuantity: Long, sender: KeyPair): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] =
    issueReissueBurnGeneratorP(issueQuantity, issueQuantity, issueQuantity, sender)

  def versionGen(builder: TransactionParser): Gen[Byte] = {
    Gen.oneOf(builder.supportedVersions.toSeq)
  }

  def createIssue(
      issuer: KeyPair,
      assetName: Array[Byte],
      description: Array[Byte],
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long
  ): Gen[IssueTransaction] = {
    val issuev1 = IssueTransactionV1.selfSigned(issuer, assetName, description, quantity, decimals, reissuable, fee, timestamp).explicitGet()
    val issuev2 = IssueTransactionV2
      .selfSigned(AddressScheme.current.chainId, issuer, assetName, description, quantity, decimals, reissuable, None, fee, timestamp)
      .right
      .get
    Gen.oneOf(Seq(issuev1, issuev2))
  }

  def createReissue(
      reissuer: KeyPair,
      assetId: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long
  ): Gen[ReissueTransaction] = {
    for {
      tx <- Gen.oneOf(
        ReissueTransactionV1
          .selfSigned(reissuer, assetId, quantity, reissuable, fee, timestamp)
          .right
          .get,
        ReissueTransactionV2
          .selfSigned(AddressScheme.current.chainId, reissuer, assetId, quantity, reissuable, fee, timestamp)
          .right
          .get
      )
    } yield tx
  }

  def createBurn(burner: KeyPair, assetId: IssuedAsset, amount: Long, fee: Long, timestamp: Long): Gen[BurnTransaction] = {
    for {
      tx <- Gen.oneOf(
        BurnTransactionV1.selfSigned(burner, assetId, amount, fee, timestamp).explicitGet(),
        BurnTransactionV2.selfSigned(AddressScheme.current.chainId, burner, assetId, amount, fee, timestamp).explicitGet()
      )
    } yield tx
  }

  def issueReissueBurnGeneratorP(
      issueQuantity: Long,
      reissueQuantity: Long,
      burnQuantity: Long,
      sender: KeyPair
  ): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] =
    for {
      (_, assetName, description, _, decimals, reissuable, iFee, timestamp) <- issueParamGen
      reissuable2                                                           <- Arbitrary.arbitrary[Boolean]
      fee                                                                   <- smallFeeGen
      issue                                                                 <- createIssue(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp)
      reissue                                                               <- createReissue(sender, IssuedAsset(issue.assetId), reissueQuantity, reissuable2, fee, timestamp)
      burn                                                                  <- createBurn(sender, IssuedAsset(issue.assetId), burnQuantity, fee, timestamp)
    } yield (issue, reissue, burn)

  val issueWithInvalidReissuesGen: Gen[(IssueTransactionV1, ReissueTransactionV1, ReissueTransactionV1)] = for {
    (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
    fee                                                                      <- smallFeeGen
  } yield {
    val issue    = IssueTransactionV1.selfSigned(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).explicitGet()
    val reissue1 = ReissueTransactionV1.selfSigned(sender, IssuedAsset(issue.assetId), quantity, reissuable = false, fee, timestamp).explicitGet()
    val reissue2 =
      ReissueTransactionV1.selfSigned(sender, IssuedAsset(issue.assetId), quantity, reissuable = true, fee, timestamp + 1).explicitGet()
    (issue, reissue1, reissue2)
  }

  def issueGen(sender: KeyPair, fixedQuantity: Option[Long] = None): Gen[IssueTransactionV1] =
    for {
      (_, assetName, description, quantity, decimals, _, _, timestamp) <- issueParamGen
    } yield {
      IssueTransactionV1
        .selfSigned(
          sender,
          assetName,
          description,
          fixedQuantity.getOrElse(quantity),
          decimals,
          reissuable = false,
          1 * Constants.UnitsInWave,
          timestamp
        )
        .right
        .get
    }

  def issueGen(sender: KeyPair, timestamp: Long): Gen[IssueTransactionV1] =
    for {
      (_, assetName, description, quantity, decimals, _, _, _) <- issueParamGen
    } yield {
      IssueTransactionV1
        .selfSigned(
          sender,
          assetName,
          description,
          quantity,
          decimals,
          reissuable = false,
          1 * Constants.UnitsInWave,
          timestamp
        )
        .right
        .get
    }

  val issueGen: Gen[IssueTransaction]     = issueReissueBurnGen.map(_._1)
  val reissueGen: Gen[ReissueTransaction] = issueReissueBurnGen.map(_._2)
  val burnGen: Gen[BurnTransaction]       = issueReissueBurnGen.map(_._3)

  def sponsorFeeCancelSponsorFeeGen(sender: KeyPair): Gen[(IssueTransaction, SponsorFeeTransaction, SponsorFeeTransaction, SponsorFeeTransaction)] =
    for {
      (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
      issue = IssueTransactionV1
        .selfSigned(sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp)
        .right
        .get
      minFee  <- smallFeeGen
      minFee1 <- smallFeeGen
      assetId = IssuedAsset(issue.assetId)
    } yield (
      issue,
      SponsorFeeTransaction.selfSigned(sender, assetId, Some(minFee), 1 * Constants.UnitsInWave, timestamp).explicitGet(),
      SponsorFeeTransaction.selfSigned(sender, assetId, Some(minFee1), 1 * Constants.UnitsInWave, timestamp).explicitGet(),
      SponsorFeeTransaction.selfSigned(sender, assetId, None, 1 * Constants.UnitsInWave, timestamp).explicitGet()
    )

  val sponsorFeeGen = for {
    sender        <- accountGen
    (_, tx, _, _) <- sponsorFeeCancelSponsorFeeGen(sender)
  } yield {
    tx
  }
  val cancelFeeSponsorshipGen = for {
    sender        <- accountGen
    (_, _, _, tx) <- sponsorFeeCancelSponsorFeeGen(sender)
  } yield {
    tx
  }

  val argGen: Gen[EXPR] = Gen.const(CONST_LONG(11))

  val funcCallGen = for {
    functionName <- genBoundedString(1, 32).map(_.toString)
    amt          <- Gen.choose(0, ContractLimits.MaxInvokeScriptArgs)
    args         <- Gen.listOfN(amt, argGen)

  } yield FUNCTION_CALL(FunctionHeader.User(functionName), args)

  val invokeScriptGen = for {
    sender      <- accountGen
    dappAddress <- accountGen
    fc          <- funcCallGen
    po <- Gen.option(for {
      asset <- Gen.option(bytes32gen.map(ByteStr(_))).map(Asset.fromCompatId)
      amt   <- positiveLongGen
    } yield InvokeScriptTransaction.Payment(amt, asset))
    fee       <- smallFeeGen
    timestamp <- timestampGen
  } yield InvokeScriptTransaction.selfSigned(sender, dappAddress.toAddress, Some(fc), po.toSeq, fee, Waves, timestamp).explicitGet()

  val priceGen: Gen[Long]            = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherAmountGen: Gen[Long]    = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherFeeAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)

  val orderTypeGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderParamGen = for {
    sender     <- accountGen
    matcher    <- accountGen
    pair       <- assetPairGen
    orderType  <- orderTypeGen
    amount     <- matcherAmountGen
    price      <- priceGen
    timestamp  <- timestampGen
    expiration <- maxOrderTimeGen
    matcherFee <- matcherFeeAmountGen
  } yield (sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee)

  val orderV1Gen: Gen[Order] = for {
    (sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee) <- orderParamGen
  } yield Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee, 1: Byte)

  val orderV2Gen: Gen[Order] = for {
    (sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee) <- orderParamGen
  } yield Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, 2: Byte)

  val orderV3Gen: Gen[Order] = for {
    (sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee) <- orderParamGen
    matcherFeeAssetId                                                                    <- assetIdGen
  } yield Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, 3: Byte, Asset.fromCompatId(matcherFeeAssetId))

  val orderGen: Gen[Order] = Gen.oneOf(orderV1Gen, orderV2Gen, orderV3Gen)

  val arbitraryOrderGen: Gen[Order] = for {
    (sender, matcher, pair, orderType, _, _, _, _, _) <- orderParamGen
    amount                                            <- Arbitrary.arbitrary[Long]
    price                                             <- Arbitrary.arbitrary[Long]
    timestamp                                         <- Arbitrary.arbitrary[Long]
    expiration                                        <- Arbitrary.arbitrary[Long]
    matcherFee                                        <- Arbitrary.arbitrary[Long]
  } yield Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, 1: Byte)

  val exchangeTransactionGen: Gen[ExchangeTransaction] = for {
    sender1                 <- accountGen
    sender2                 <- accountGen
    assetPair               <- assetPairGen
    buyerAnotherAsset       <- assetIdGen.map(Asset.fromCompatId)
    sellerAnotherAsset      <- assetIdGen.map(Asset.fromCompatId)
    buyerMatcherFeeAssetId  <- Gen.oneOf(assetPair.amountAsset, assetPair.priceAsset, buyerAnotherAsset, Waves)
    sellerMatcherFeeAssetId <- Gen.oneOf(assetPair.amountAsset, assetPair.priceAsset, sellerAnotherAsset, Waves)
    r <- Gen.oneOf(
      exchangeV1GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset),
      exchangeV2GeneratorP(
        buyer = sender1,
        seller = sender2,
        amountAssetId = assetPair.amountAsset,
        priceAssetId = assetPair.priceAsset,
        buyMatcherFeeAssetId = buyerMatcherFeeAssetId,
        sellMatcherFeeAssetId = sellerMatcherFeeAssetId
      )
    )
  } yield r

  def exchangeGeneratorP(
      buyer: KeyPair,
      seller: KeyPair,
      amountAssetId: Asset,
      priceAssetId: Asset,
      fixedMatcherFee: Option[Long] = None
  ): Gen[ExchangeTransaction] = {
    Gen.oneOf(
      exchangeV1GeneratorP(buyer, seller, amountAssetId, priceAssetId),
      exchangeV2GeneratorP(buyer, seller, amountAssetId, priceAssetId)
    )
  }

  def exchangeV1GeneratorP(
      buyer: KeyPair,
      seller: KeyPair,
      amountAssetId: Asset,
      priceAssetId: Asset,
      fixedMatcherFee: Option[Long] = None
  ): Gen[ExchangeTransaction] =
    for {
      (_, matcher, _, _, amount1, price, timestamp, expiration, genMatcherFee) <- orderParamGen
      amount2: Long                                                            <- matcherAmountGen
      matchedAmount: Long                                                      <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
      assetPair = AssetPair(amountAssetId, priceAssetId)
    } yield {
      val matcherFee = fixedMatcherFee.getOrElse(genMatcherFee)
      val o1         = Order.buy(buyer, matcher, assetPair, amount1, price, timestamp, expiration, matcherFee, 1: Byte)
      val o2         = Order.sell(seller, matcher, assetPair, amount2, price, timestamp, expiration, matcherFee, 1: Byte)
      val buyFee     = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
      val sellFee    = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()
      val trans =
        ExchangeTransactionV1
          .create(
            matcher,
            o1.asInstanceOf[OrderV1],
            o2.asInstanceOf[OrderV1],
            matchedAmount,
            price,
            buyFee,
            sellFee,
            (buyFee + sellFee) / 2,
            expiration - 100
          )
          .explicitGet()

      trans
    }

  private type OrderConstructor = (KeyPair, PublicKey, AssetPair, Long, Long, Long, Long, Long) => Order

  def exchangeV2GeneratorP(
      buyer: KeyPair,
      seller: KeyPair,
      amountAssetId: Asset,
      priceAssetId: Asset,
      fixedMatcherFee: Option[Long] = None,
      orderVersions: Set[Byte] = Set(1, 2, 3),
      buyMatcherFeeAssetId: Asset = Waves,
      sellMatcherFeeAssetId: Asset = Waves,
      fixedMatcher: Option[KeyPair] = None
  ): Gen[ExchangeTransactionV2] = {

    def mkBuyOrder(version: Byte): OrderConstructor = version match {
      case 1 => OrderV1.buy
      case 2 => OrderV2.buy
      case 3 => OrderV3.buy(_, _, _, _, _, _, _, _, buyMatcherFeeAssetId)
    }

    def mkSellOrder(version: Byte): OrderConstructor = version match {
      case 1 => OrderV1.sell
      case 2 => OrderV2.sell
      case 3 => OrderV3.sell(_, _, _, _, _, _, _, _, sellMatcherFeeAssetId)
    }

    for {
      (_, generatedMatcher, _, _, amount1, price, timestamp, expiration, generatedMatcherFee) <- orderParamGen
      amount2: Long                                                                           <- matcherAmountGen
      matcher    = fixedMatcher.getOrElse(generatedMatcher)
      matcherFee = fixedMatcherFee.getOrElse(generatedMatcherFee)
      matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
      assetPair = AssetPair(amountAssetId, priceAssetId)
      mkO1 <- Gen.oneOf(orderVersions.map(mkBuyOrder).toSeq)
      mkO2 <- Gen.oneOf(orderVersions.map(mkSellOrder).toSeq)
    } yield {

      val buyFee  = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
      val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()

      val o1 = mkO1(buyer, matcher, assetPair, amount1, price, timestamp, expiration, matcherFee)
      val o2 = mkO2(seller, matcher, assetPair, amount2, price, timestamp, expiration, matcherFee)

      ExchangeTransactionV2
        .create(matcher, o1, o2, matchedAmount, price, buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100)
        .explicitGet()
    }
  }

  val exchangeTransactionV2WithArbitraryFeeAssetsInOrdersGen: Gen[ExchangeTransaction] =
    for {
      buyer                   <- accountGen
      seller                  <- accountGen
      matcher                 <- accountGen
      pair                    <- assetPairGen
      amount                  <- matcherAmountGen
      price                   <- priceGen
      buyerTimestamp          <- timestampGen
      sellerTimestamp         <- timestampGen
      buyerExpiration         <- maxOrderTimeGen
      sellerExpiration        <- maxOrderTimeGen
      buyerMatcherFee         <- matcherFeeAmountGen
      sellerMatcherFee        <- matcherFeeAmountGen
      buyerMatcherFeeAssetId  <- bytes32gen map (b => IssuedAsset(ByteStr(b)))
      sellerMatcherFeeAssetId <- bytes32gen map (b => IssuedAsset(ByteStr(b)))
    } yield {

      val buyOrder = OrderV3(
        buyer,
        matcher,
        pair,
        OrderType.BUY,
        amount,
        price,
        buyerTimestamp,
        buyerExpiration,
        buyerMatcherFee,
        Asset fromCompatId buyerMatcherFeeAssetId.compatId
      )

      val sellOrder = OrderV3(
        seller,
        matcher,
        pair,
        OrderType.SELL,
        amount,
        price,
        sellerTimestamp,
        sellerExpiration,
        sellerMatcherFee,
        Asset fromCompatId sellerMatcherFeeAssetId.compatId
      )

      ExchangeTransactionV2
        .create(
          matcher,
          buyOrder,
          sellOrder,
          amount,
          price,
          buyOrder.matcherFee,
          sellOrder.matcherFee,
          300000L,
          System.currentTimeMillis() - 10000L
        )
        .explicitGet()
    }

  val randomTransactionGen: Gen[ProvenTransaction] = (for {
    tr <- transferV1Gen
    (is, ri, bu) <- issueReissueBurnGen.retryUntil {
      case (i, r, b) => i.version == 1 && r.version == 1 && b.version == 1
    }
    ca <- createAliasGen.retryUntil(_.version == 1).map(_.asInstanceOf[CreateAliasTransactionV1])
    xt <- exchangeTransactionGen
    tx <- Gen.oneOf(tr, is.asInstanceOf[IssueTransactionV1], ri.asInstanceOf[ReissueTransactionV1], ca, bu.asInstanceOf[BurnTransactionV1], xt)
  } yield tx).label("random transaction")

  def randomTransactionsGen(count: Int): Gen[Seq[ProvenTransaction]] =
    for {
      transactions <- Gen.listOfN(count, randomTransactionGen)
    } yield transactions

  val genesisGen: Gen[GenesisTransaction] = accountGen.flatMap(acc => genesisGeneratorP(acc.toAddress))

  def genesisGeneratorP(recipient: Address): Gen[GenesisTransaction] =
    for {
      amt <- Gen.choose(1, 100000000L * 100000000L)
      ts  <- positiveIntGen
    } yield GenesisTransaction.create(recipient, amt, ts).explicitGet()

  import DataEntry.MaxKeySize

  val dataKeyGen = for {
    size <- Gen.choose[Byte](1, MaxKeySize)
  } yield Random.nextString(size)

  val dataScriptsKeyGen = for {
    size <- Gen.choose[Byte](1, 10)
  } yield Random.nextString(size)

  val dataAsciiKeyGen = for {
    size <- Gen.choose[Byte](1, MaxKeySize)
  } yield Random.alphanumeric.take(size).mkString

  def longEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[IntegerDataEntry] =
    for {
      key   <- keyGen
      value <- Gen.choose[Long](Long.MinValue, Long.MaxValue)
    } yield IntegerDataEntry(key, value)

  def booleanEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[BooleanDataEntry] =
    for {
      key   <- keyGen
      value <- Gen.oneOf(true, false)
    } yield BooleanDataEntry(key, value)

  def binaryEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[BinaryDataEntry] =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- byteArrayGen(size)
    } yield BinaryDataEntry(key, ByteStr(value))

  def stringEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[StringDataEntry] =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- Gen.listOfN(size, aliasAlphabetGen)
    } yield StringDataEntry(key, value.mkString)

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[DataEntry[_]] =
    Gen.oneOf(longEntryGen(keyGen), booleanEntryGen(keyGen), binaryEntryGen(maxSize, keyGen), stringEntryGen(maxSize, keyGen))

  val dataTransactionGen: Gen[DataTransaction] = dataTransactionGen(DataTransaction.MaxEntryCount)

  def dataTransactionGen(maxEntryCount: Int, useForScript: Boolean = false): Gen[DataTransaction] =
    (for {
      sender    <- accountGen
      timestamp <- timestampGen
      size      <- Gen.choose(0, maxEntryCount)
      maxEntrySize = if (useForScript) 200 else (DataTransaction.MaxBytes - 122) / (size max 1) min DataEntry.MaxValueSize
      data <- if (useForScript) Gen.listOfN(size, dataEntryGen(maxEntrySize, dataScriptsKeyGen)) else Gen.listOfN(size, dataEntryGen(maxEntrySize))
      uniq = data.foldRight(List.empty[DataEntry[_]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
    } yield DataTransaction.selfSigned(sender, uniq, 15000000, timestamp).explicitGet())
      .label("DataTransaction")

  def dataTransactionGenP(sender: KeyPair, data: List[DataEntry[_]]): Gen[DataTransaction] =
    (for {
      timestamp <- timestampGen
    } yield DataTransaction.selfSigned(sender, data, 15000000, timestamp).explicitGet())
      .label("DataTransactionP")

  def preconditionsTransferAndLease(typed: EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ExprScript(typed).explicitGet())
      transfer  <- transferGeneratorPV2(ts, master, recipient.toAddress, ENOUGH_AMT / 2)
      fee       <- smallFeeGen
      lease = LeaseTransactionV2.selfSigned(master, ENOUGH_AMT / 2, fee, ts, recipient).explicitGet()
    } yield (genesis, setScript, lease, transfer)

  def smartIssueTransactionGen(senderGen: Gen[KeyPair] = accountGen, sGen: Gen[Option[Script]] = Gen.option(scriptGen)): Gen[IssueTransactionV2] =
    for {
      script                                                                      <- sGen
      (_, assetName, description, quantity, decimals, reissuable, fee, timestamp) <- issueParamGen
      sender                                                                      <- senderGen
    } yield IssueTransactionV2
      .selfSigned(AddressScheme.current.chainId, sender, assetName, description, quantity, decimals, reissuable, script, fee, timestamp)
      .explicitGet()
}
