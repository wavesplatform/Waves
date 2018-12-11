package com.wavesplatform

import cats.syntax.semigroup._
import com.wavesplatform.account.PublicKeyAccount._
import com.wavesplatform.account._
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.settings.Constants
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import MassTransferTransaction.MaxTransferCount
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import org.scalacheck.Gen.{alphaLowerChar, alphaUpperChar, frequency, numChar}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import scala.util.Random

trait TransactionGen extends TransactionGenBase { _: Suite =>

}

trait TransactionGenBase extends ScriptGen with NTPTime { _: Suite =>

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

  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(ntpTime.correctedTime() - _)

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))

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
  } yield Alias.buildWithCurrentNetworkByte(str.mkString).explicitGet()

  val invalidAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, invalidAliasAlphabetGen)
  } yield aliasChars.mkString

  val accountOrAliasGen: Gen[AddressOrAlias] = Gen.oneOf(aliasGen, accountGen.map(PublicKeyAccount.toAddress(_)))

  def otherAccountGen(candidate: PrivateKeyAccount): Gen[PrivateKeyAccount] = accountGen.flatMap(Gen.oneOf(candidate, _))

  val positiveLongGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L / 100)
  val positiveIntGen: Gen[Int]   = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long]     = Gen.choose(400000, 100000000)

  val maxOrderTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + ntpTime.correctedTime())
  val timestampGen: Gen[Long]    = Gen.choose(1, Long.MaxValue - 100)

  val wavesAssetGen: Gen[Option[ByteStr]] = Gen.const(None)
  val assetIdGen: Gen[Option[ByteStr]]    = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen.map(ByteStr(_)))))

  val assetPairGen = assetIdGen.flatMap {
    case None => bytes32gen.map(b => AssetPair(None, Some(ByteStr(b))))
    case a1 @ Some(a1bytes) =>
      val a2bytesGen = byteArrayGen(31).map(a2bytes => Option((~a1bytes.arr(0)).toByte +: a2bytes))
      Gen.oneOf(Gen.const(None), a2bytesGen).map(a2 => AssetPair(a1, a2.map(ByteStr(_))))
  }

  val proofsGen: Gen[Proofs] = for {
    proofsAmount <- Gen.choose(1, 8)
    proofs       <- Gen.listOfN(proofsAmount, genBoundedBytes(0, 50))
  } yield Proofs.create(proofs.map(ByteStr(_))).explicitGet()

  val scriptGen = BOOLgen(100).map {
    case (expr, _) =>
      val typed =
        CompilerV1(PureContext.build(V1).compilerContext |+| CryptoContext.compilerContext(Global), expr).explicitGet()
      ScriptV1(typed._1).explicitGet()
  }

  val setAssetScriptTransactionGen: Gen[(Seq[Transaction], SetAssetScriptTransaction)] = for {
    version                                                                  <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
    fee                                                                      <- smallFeeGen
    timestamp                                                                <- timestampGen
    proofs                                                                   <- proofsGen
    script                                                                   <- Gen.option(scriptGen)
    issue = IssueTransactionV2
      .selfSigned(2: Byte,
                  AddressScheme.current.chainId,
                  sender,
                  assetName,
                  description,
                  quantity,
                  decimals,
                  reissuable = true,
                  script,
                  iFee,
                  timestamp)
      .explicitGet()
  } yield
    (Seq(issue),
     SetAssetScriptTransaction
       .create(version, AddressScheme.current.chainId, sender, issue.id(), script, 1 * Constants.UnitsInWave + ScriptExtraFee, timestamp, proofs)
       .explicitGet())

  val setScriptTransactionGen: Gen[SetScriptTransaction] = for {
    version                   <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    sender: PrivateKeyAccount <- accountGen
    fee                       <- smallFeeGen
    timestamp                 <- timestampGen
    proofs                    <- proofsGen
    script                    <- Gen.option(scriptGen)
  } yield SetScriptTransaction.create(version, sender, script, fee, timestamp, proofs).explicitGet()

  def selfSignedSetScriptTransactionGenP(sender: PrivateKeyAccount, s: Script): Gen[SetScriptTransaction] =
    for {
      version   <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield SetScriptTransaction.selfSigned(version, sender, Some(s), fee, timestamp).explicitGet()

  val paymentGen: Gen[PaymentTransaction] = for {
    sender: PrivateKeyAccount    <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    tx                           <- paymentGeneratorP(sender, recipient)
  } yield tx

  val selfPaymentGen: Gen[PaymentTransaction] = accountGen.flatMap(acc => paymentGeneratorP(acc, acc))

  def paymentGeneratorP(sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
    timestampGen.flatMap(ts => paymentGeneratorP(ts, sender, recipient))

  def paymentGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
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

  def createLease(sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias) = {
    val v1 = LeaseTransactionV1.selfSigned(sender, amount, fee, timestamp, recipient).explicitGet()
    val v2 = LeaseTransactionV2.selfSigned(LeaseTransactionV2.supportedVersions.head, sender, amount, fee, timestamp, recipient).explicitGet()
    Gen.oneOf(v1, v2)
  }

  def createLeaseCancel(sender: PrivateKeyAccount, leaseId: ByteStr, cancelFee: Long, timestamp: Long) = {
    val v1 = LeaseCancelTransactionV1.selfSigned(sender, leaseId, cancelFee, timestamp + 1).explicitGet()
    val v2 = LeaseCancelTransactionV2
      .selfSigned(LeaseTransactionV2.supportedVersions.head, AddressScheme.current.chainId, sender, leaseId, cancelFee, timestamp + 1)
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

  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount,
                               recipient: AddressOrAlias,
                               unleaseSender: PrivateKeyAccount): Gen[(LeaseTransaction, LeaseCancelTransaction)] =
    for {
      (_, amount, fee, timestamp, _) <- leaseParamGen
      lease                          <- createLease(leaseSender, amount, fee, timestamp, recipient)
      fee2                           <- smallFeeGen
      unlease                        <- createLeaseCancel(unleaseSender, lease.id(), fee2, timestamp + 1)
    } yield (lease, unlease)

  val twoLeasesGen: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    amount2                                     <- positiveLongGen
    recipient2: PrivateKeyAccount               <- accountGen
    fee2                                        <- smallFeeGen
    lease1                                      <- createLease(sender, amount, fee, timestamp, recipient)
    lease2                                      <- createLease(sender, amount2, fee2, timestamp + 1, recipient2)
  } yield (lease1, lease2)

  val leaseAndCancelWithOtherSenderGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    otherSender: PrivateKeyAccount              <- accountGen
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
  } yield (assetId.map(ByteStr(_)), sender, recipient, amount, timestamp, feeAssetId.map(ByteStr(_)), feeAmount, attachment)

  def transferGeneratorP(sender: PrivateKeyAccount,
                         recipient: AddressOrAlias,
                         assetId: Option[AssetId],
                         feeAssetId: Option[AssetId]): Gen[TransferTransactionV1] =
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet()

  def versionedTransferGeneratorP(sender: PrivateKeyAccount,
                                  recipient: AddressOrAlias,
                                  assetId: Option[AssetId],
                                  feeAssetId: Option[AssetId]): Gen[TransferTransactionV2] =
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield
      TransferTransactionV2
        .selfSigned(TransferTransactionV2.supportedVersions.head, assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment)
        .explicitGet()

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransactionV1] =
    for {
      amount                                    <- Gen.choose(1, maxAmount)
      (_, _, _, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV1.selfSigned(None, sender, recipient, amount, timestamp, None, feeAmount, attachment).explicitGet()

  def transferGeneratorPV2(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransactionV2] =
    for {
      amount                                    <- Gen.choose(1, maxAmount)
      (_, _, _, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV2.selfSigned(2, None, sender, recipient, amount, timestamp, None, feeAmount, attachment).explicitGet()

  def transferGeneratorP(timestamp: Long,
                         sender: PrivateKeyAccount,
                         recipient: AddressOrAlias,
                         assetId: Option[AssetId],
                         feeAssetId: Option[AssetId]): Gen[TransferTransactionV1] =
    for {
      (_, _, _, amount, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet()

  def wavesTransferGeneratorP(sender: PrivateKeyAccount, recipient: AddressOrAlias): Gen[TransferTransactionV1] =
    transferGeneratorP(sender, recipient, None, None)

  def wavesTransferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias): Gen[TransferTransactionV1] =
    transferGeneratorP(timestamp, sender, recipient, None, None)

  def massTransferGeneratorP(sender: PrivateKeyAccount, transfers: List[ParsedTransfer], assetId: Option[AssetId]): Gen[MassTransferTransaction] =
    for {
      version                                           <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (_, _, _, _, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield MassTransferTransaction.selfSigned(version, assetId, sender, transfers, timestamp, feeAmount, attachment).explicitGet()

  def createWavesTransfer(sender: PrivateKeyAccount,
                          recipient: Address,
                          amount: Long,
                          fee: Long,
                          timestamp: Long): Either[ValidationError, TransferTransactionV1] =
    TransferTransactionV1.selfSigned(None, sender, recipient, amount, timestamp, None, fee, Array())

  val transferV1Gen = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet())
    .label("transferTransaction")

  val transferV2Gen = (for {
    version                                                                            <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
    proofs                                                                             <- proofsGen
  } yield
    TransferTransactionV2
      .create(version, assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, proofs)
      .explicitGet())
    .label("VersionedTransferTransaction")

  def versionedTransferGenP(sender: PublicKeyAccount, recipient: Address, proofs: Proofs) =
    (for {
      version   <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TransferTransactionV2.create(version, None, sender, recipient, amt, timestamp, None, fee, Array.emptyByteArray, proofs).explicitGet())
      .label("VersionedTransferTransactionP")

  val transferWithWavesFeeGen = for {
    (assetId, sender, recipient, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, recipient, amount, timestamp, None, feeAmount, attachment).explicitGet()

  val selfTransferWithWavesFeeGen: Gen[TransferTransactionV1] = for {
    (assetId, sender, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, sender, amount, timestamp, None, feeAmount, attachment).explicitGet()

  val selfTransferGen: Gen[TransferTransactionV1] = for {
    (assetId, sender, _, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV1.selfSigned(assetId, sender, sender, amount, timestamp, feeAssetId, feeAmount, attachment).explicitGet()

  val massTransferGen: Gen[MassTransferTransaction] = massTransferGen(MaxTransferCount)

  def massTransferGen(maxTransfersCount: Int) =
    for {
      version                                                      <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (assetId, sender, _, _, timestamp, _, feeAmount, attachment) <- transferParamGen
      transferCount                                                <- Gen.choose(0, maxTransfersCount)
      transferGen = for {
        recipient <- accountOrAliasGen
        amount    <- Gen.choose(1L, Long.MaxValue / maxTransfersCount)
      } yield ParsedTransfer(recipient, amount)
      recipients <- Gen.listOfN(transferCount, transferGen)
    } yield MassTransferTransaction.selfSigned(version, assetId, sender, recipients, timestamp, feeAmount, attachment).explicitGet()

  val MinIssueFee = 100000000

  val createAliasGen: Gen[CreateAliasTransaction] = for {
    timestamp: Long           <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias              <- aliasGen
    version                   <- Gen.oneOf(CreateAliasTransactionV2.supportedVersions.toSeq)
    tx <- Gen.oneOf(
      CreateAliasTransactionV1.selfSigned(sender, alias, MinIssueFee, timestamp).explicitGet(),
      CreateAliasTransactionV2.selfSigned(sender, version, alias, MinIssueFee, timestamp).explicitGet()
    )
  } yield tx

  def createAliasGen(sender: PrivateKeyAccount, alias: Alias, fee: Long, timestamp: Long): Gen[CreateAliasTransaction] = {
    for {
      version <- Gen.oneOf(CreateAliasTransactionV2.supportedVersions.toSeq)
      tx <- Gen.oneOf(
        CreateAliasTransactionV1.selfSigned(sender, alias, fee, timestamp).explicitGet(),
        CreateAliasTransactionV2.selfSigned(sender, version, alias, fee, timestamp).explicitGet()
      )
    } yield tx
  }

  val issueParamGen = for {
    sender: PrivateKeyAccount <- accountGen
    assetName                 <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description               <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    quantity                  <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
    decimals                  <- Gen.choose(0: Byte, 8: Byte)
    reissuable                <- Arbitrary.arbitrary[Boolean]
    fee                       <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp                 <- positiveLongGen
  } yield (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp)

  val issueReissueBurnGen: Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    amount                    <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    r                         <- issueReissueBurnGeneratorP(amount, amount, amount, sender)
  } yield r

  def issueReissueBurnGeneratorP(issueQuantity: Long, sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] =
    issueReissueBurnGeneratorP(issueQuantity, issueQuantity, issueQuantity, sender)

  def versionGen(builder: TransactionParser): Gen[Byte] = {
    Gen.oneOf(builder.supportedVersions.toSeq)
  }

  def createIssue(issuer: PrivateKeyAccount,
                  assetName: Array[Byte],
                  description: Array[Byte],
                  quantity: Long,
                  decimals: Byte,
                  reissuable: Boolean,
                  fee: Long,
                  timestamp: Long): Gen[IssueTransaction] = {
    val issuev1 = IssueTransactionV1.selfSigned(issuer, assetName, description, quantity, decimals, reissuable, fee, timestamp).explicitGet()
    val issuev2 = IssueTransactionV2
      .selfSigned(2, AddressScheme.current.chainId, issuer, assetName, description, quantity, decimals, reissuable, None, fee, timestamp)
      .right
      .get
    Gen.oneOf(Seq(issuev1, issuev2))
  }

  def createReissue(reissuer: PrivateKeyAccount,
                    assetId: ByteStr,
                    quantity: Long,
                    reissuable: Boolean,
                    fee: Long,
                    timestamp: Long): Gen[ReissueTransaction] = {
    for {
      version <- versionGen(ReissueTransactionV2)
      tx <- Gen.oneOf(
        ReissueTransactionV1
          .selfSigned(reissuer, assetId, quantity, reissuable, fee, timestamp)
          .right
          .get,
        ReissueTransactionV2
          .selfSigned(version, AddressScheme.current.chainId, reissuer, assetId, quantity, reissuable, fee, timestamp)
          .right
          .get
      )
    } yield tx
  }

  def createBurn(burner: PrivateKeyAccount, assetId: AssetId, amount: Long, fee: Long, timestamp: Long): Gen[BurnTransaction] = {
    for {
      version <- versionGen(BurnTransactionV2)
      tx <- Gen.oneOf(
        BurnTransactionV1.selfSigned(burner, assetId, amount, fee, timestamp).explicitGet(),
        BurnTransactionV2.selfSigned(version, AddressScheme.current.chainId, burner, assetId, amount, fee, timestamp).explicitGet()
      )
    } yield tx
  }

  def issueReissueBurnGeneratorP(issueQuantity: Long,
                                 reissueQuantity: Long,
                                 burnQuantity: Long,
                                 sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] =
    for {
      (_, assetName, description, _, decimals, reissuable, iFee, timestamp) <- issueParamGen
      reissuable2                                                           <- Arbitrary.arbitrary[Boolean]
      fee                                                                   <- smallFeeGen
      issue                                                                 <- createIssue(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp)
      reissue                                                               <- createReissue(sender, issue.assetId(), reissueQuantity, reissuable2, fee, timestamp)
      burn                                                                  <- createBurn(sender, issue.assetId(), burnQuantity, fee, timestamp)
    } yield (issue, reissue, burn)

  val issueWithInvalidReissuesGen: Gen[(IssueTransactionV1, ReissueTransactionV1, ReissueTransactionV1)] = for {
    (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
    fee                                                                      <- smallFeeGen
  } yield {
    val issue    = IssueTransactionV1.selfSigned(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).explicitGet()
    val reissue1 = ReissueTransactionV1.selfSigned(sender, issue.assetId(), quantity, reissuable = false, fee, timestamp).explicitGet()
    val reissue2 = ReissueTransactionV1.selfSigned(sender, issue.assetId(), quantity, reissuable = true, fee, timestamp + 1).explicitGet()
    (issue, reissue1, reissue2)
  }

  def issueGen(sender: PrivateKeyAccount, fixedQuantity: Option[Long] = None): Gen[IssueTransactionV1] =
    for {
      (_, assetName, description, quantity, decimals, _, _, timestamp) <- issueParamGen
    } yield {
      IssueTransactionV1
        .selfSigned(sender,
                    assetName,
                    description,
                    fixedQuantity.getOrElse(quantity),
                    decimals,
                    reissuable = false,
                    1 * Constants.UnitsInWave,
                    timestamp)
        .right
        .get
    }

  val issueGen: Gen[IssueTransaction]     = issueReissueBurnGen.map(_._1)
  val reissueGen: Gen[ReissueTransaction] = issueReissueBurnGen.map(_._2)
  val burnGen: Gen[BurnTransaction]       = issueReissueBurnGen.map(_._3)

  def sponsorFeeCancelSponsorFeeGen(
      sender: PrivateKeyAccount): Gen[(IssueTransaction, SponsorFeeTransaction, SponsorFeeTransaction, SponsorFeeTransaction)] =
    for {
      (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
      issue = IssueTransactionV1
        .selfSigned(sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp)
        .right
        .get
      minFee  <- smallFeeGen
      minFee1 <- smallFeeGen
      assetId = issue.assetId()
    } yield
      (issue,
       SponsorFeeTransaction.selfSigned(1, sender, assetId, Some(minFee), 1 * Constants.UnitsInWave, timestamp).explicitGet(),
       SponsorFeeTransaction.selfSigned(1, sender, assetId, Some(minFee1), 1 * Constants.UnitsInWave, timestamp).explicitGet(),
       SponsorFeeTransaction.selfSigned(1, sender, assetId, None, 1 * Constants.UnitsInWave, timestamp).explicitGet(),
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

  val orderGen: Gen[Order] = Gen.oneOf(orderV1Gen, orderV2Gen)

  val arbitraryOrderGen: Gen[Order] = for {
    (sender, matcher, pair, orderType, _, _, _, _, _) <- orderParamGen
    amount                                            <- Arbitrary.arbitrary[Long]
    price                                             <- Arbitrary.arbitrary[Long]
    timestamp                                         <- Arbitrary.arbitrary[Long]
    expiration                                        <- Arbitrary.arbitrary[Long]
    matcherFee                                        <- Arbitrary.arbitrary[Long]
  } yield Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, 1: Byte)

  val exchangeTransactionGen: Gen[ExchangeTransaction] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    assetPair                  <- assetPairGen
    r <- Gen.oneOf(
      exchangeV1GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset),
      exchangeV2GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset)
    )
  } yield r

  def exchangeGeneratorP(buyer: PrivateKeyAccount,
                         seller: PrivateKeyAccount,
                         amountAssetId: Option[ByteStr],
                         priceAssetId: Option[ByteStr],
                         fixedMatcherFee: Option[Long] = None): Gen[ExchangeTransaction] = {
    Gen.oneOf(
      exchangeV1GeneratorP(buyer, seller, amountAssetId, priceAssetId),
      exchangeV2GeneratorP(buyer, seller, amountAssetId, priceAssetId)
    )
  }

  def exchangeV1GeneratorP(buyer: PrivateKeyAccount,
                           seller: PrivateKeyAccount,
                           amountAssetId: Option[ByteStr],
                           priceAssetId: Option[ByteStr],
                           fixedMatcherFee: Option[Long] = None): Gen[ExchangeTransaction] =
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
          .create(matcher,
                  o1.asInstanceOf[OrderV1],
                  o2.asInstanceOf[OrderV1],
                  matchedAmount,
                  price,
                  buyFee,
                  sellFee,
                  (buyFee + sellFee) / 2,
                  expiration - 100)
          .explicitGet()

      trans
    }

  private type OrderConstructor = (PrivateKeyAccount, PublicKeyAccount, AssetPair, Long, Long, Long, Long, Long) => Order

  def exchangeV2GeneratorP(buyer: PrivateKeyAccount,
                           seller: PrivateKeyAccount,
                           amountAssetId: Option[ByteStr],
                           priceAssetId: Option[ByteStr],
                           fixedMatcherFee: Option[Long] = None,
                           orderVersions: Set[Byte] = Set(1, 2)): Gen[ExchangeTransactionV2] = {
    def mkBuyOrder(version: Byte): OrderConstructor  = if (version == 1) OrderV1.buy else OrderV2.buy
    def mkSellOrder(version: Byte): OrderConstructor = if (version == 1) OrderV1.sell else OrderV2.sell

    for {
      (_, matcher, _, _, price, amount1, timestamp, expiration, genMatcherFee) <- orderParamGen
      amount2: Long                                                            <- matcherAmountGen
      matcherFee = fixedMatcherFee.getOrElse(genMatcherFee)
      matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
      assetPair = AssetPair(amountAssetId, priceAssetId)
      mkO1 <- Gen.oneOf(orderVersions.map(mkBuyOrder).toSeq)
      mkO2 <- Gen.oneOf(orderVersions.map(mkSellOrder).toSeq)
    } yield {
      val buyFee  = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
      val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()

      val o1 = mkO1(seller, matcher, assetPair, amount1, price, timestamp, expiration, matcherFee)
      val o2 = mkO2(seller, matcher, assetPair, amount2, price, timestamp, expiration, matcherFee)

      ExchangeTransactionV2
        .create(matcher, o1, o2, matchedAmount, price, buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100)
        .explicitGet()
    }
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

  val genesisGen: Gen[GenesisTransaction] = accountGen.flatMap(genesisGeneratorP)

  def genesisGeneratorP(recipient: PrivateKeyAccount): Gen[GenesisTransaction] =
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

  def longEntryGen(keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      value <- Gen.choose[Long](Long.MinValue, Long.MaxValue)
    } yield IntegerDataEntry(key, value)

  def booleanEntryGen(keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      value <- Gen.oneOf(true, false)
    } yield BooleanDataEntry(key, value)

  def binaryEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- byteArrayGen(size)
    } yield BinaryDataEntry(key, ByteStr(value))

  def stringEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- Gen.listOfN(size, aliasAlphabetGen)
    } yield StringDataEntry(key, value.mkString)

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    Gen.oneOf(longEntryGen(keyGen), booleanEntryGen(keyGen), binaryEntryGen(maxSize, keyGen), stringEntryGen(maxSize, keyGen))

  val dataTransactionGen: Gen[DataTransaction] = dataTransactionGen(DataTransaction.MaxEntryCount)

  def dataTransactionGen(maxEntryCount: Int, useForScript: Boolean = false) =
    (for {
      sender    <- accountGen
      timestamp <- timestampGen
      size      <- Gen.choose(0, maxEntryCount)
      maxEntrySize = if (useForScript) 200 else (DataTransaction.MaxBytes - 122) / (size max 1) min DataEntry.MaxValueSize
      data <- if (useForScript) Gen.listOfN(size, dataEntryGen(maxEntrySize, dataScriptsKeyGen)) else Gen.listOfN(size, dataEntryGen(maxEntrySize))
      uniq = data.foldRight(List.empty[DataEntry[_]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
      version <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
    } yield DataTransaction.selfSigned(version, sender, uniq, 15000000, timestamp).explicitGet())
      .label("DataTransaction")

  def dataTransactionGenP(sender: PrivateKeyAccount, data: List[DataEntry[_]]): Gen[DataTransaction] =
    (for {
      version   <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      timestamp <- timestampGen
    } yield DataTransaction.selfSigned(version, sender, data, 15000000, timestamp).explicitGet())
      .label("DataTransactionP")

  def preconditionsTransferAndLease(typed: EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet())
      transfer  <- transferGeneratorPV2(ts, master, recipient.toAddress, ENOUGH_AMT / 2)
      fee       <- smallFeeGen
      lease = LeaseTransactionV2.selfSigned(LeaseTransactionV2.supportedVersions.head, master, ENOUGH_AMT / 2, fee, ts, recipient).explicitGet()
    } yield (genesis, setScript, lease, transfer)

  def smartIssueTransactionGen(senderGen: Gen[PrivateKeyAccount] = accountGen,
                               sGen: Gen[Option[Script]] = Gen.option(scriptGen)): Gen[IssueTransactionV2] =
    for {
      version                                                                     <- Gen.oneOf(IssueTransactionV2.supportedVersions.toSeq)
      script                                                                      <- sGen
      (_, assetName, description, quantity, decimals, reissuable, fee, timestamp) <- issueParamGen
      sender                                                                      <- senderGen
    } yield
      IssueTransactionV2
        .selfSigned(version, AddressScheme.current.chainId, sender, assetName, description, quantity, decimals, reissuable, script, fee, timestamp)
        .explicitGet()
}
