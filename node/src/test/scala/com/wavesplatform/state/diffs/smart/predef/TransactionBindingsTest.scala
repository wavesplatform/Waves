package com.wavesplatform.state.diffs.smart.predef

import cats.syntax.either.*
import com.wavesplatform.account.{Address, Alias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Testing.evaluated
import com.wavesplatform.lang.directives.values.{Asset as AssetType, *}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, GlobalValNames, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.ci.*
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, WavesEnvironment, buildThisValue}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Asset, DataTransaction, Proofs, TxHelpers, TxVersion}
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.EitherValues
import play.api.libs.json.Json
import shapeless.Coproduct

import scala.util.Random

class TransactionBindingsTest extends PropSpec with PathMockFactory with EitherValues with WithDomain {
  private val T = 'T'.toByte

  property("TransferTransaction binding") {
    Seq(
      TxHelpers.transfer(),
      TxHelpers.transfer(version = TxVersion.V1)
    ).foreach { tx =>
      // `version`  is not properly bound yet
      val result = runScript(
        s"""
           |match tx {
           | case t : TransferTransaction  =>
           |   ${provenPart(tx)}
           |   let amount = t.amount == ${tx.amount}
           |   let feeAssetId = if (${tx.feeAssetId != Waves})
           |      then extract(t.feeAssetId) == base58'${tx.feeAssetId.maybeBase58Repr.getOrElse("")}'
           |      else isDefined(t.feeAssetId) == false
           |   let assetId = if (${tx.assetId != Waves})
           |      then extract(t.assetId) == base58'${tx.assetId.maybeBase58Repr.getOrElse("")}'
           |      else isDefined(t.assetId) == false
           |   let recipient = match (t.recipient) {
           |       case a: Address => a.bytes == base58'${tx.recipient.cast[Address].fold("")(a => Base58.encode(a.bytes))}'
           |       case a: Alias => a.alias == ${Json.toJson(tx.recipient.cast[Alias].map(_.name).getOrElse(""))}
           |      }
           |    let attachment = t.attachment == base58'${Base58.encode(tx.attachment.arr)}'
           |   ${assertProvenPart("t")} && amount && feeAssetId && assetId && recipient && attachment
           | case _ => throw()
           | }
           |""".stripMargin,
        Coproduct(tx),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("IssueTransaction binding") {
    val tx = TxHelpers.issue()
    val s =
      s"""
         |match tx {
         | case t : IssueTransaction =>
         |   ${provenPart(tx)}
         |   let quantity = t.quantity == ${tx.quantity}
         |   let decimals = t.decimals == ${tx.decimals}
         |   let reissuable = t.reissuable == ${tx.reissuable}
         |   let name = t.name == base58'${Base58.encode(tx.name.toByteArray)}'
         |   let description = t.description == base58'${Base58.encode(tx.description.toByteArray)}'
         |   let script = if (${tx.script.isDefined}) then extract(t.script) == base64'${tx.script
        .fold("")(_.bytes().base64)}' else isDefined(t.script) == false
         |   ${assertProvenPart("t")} && quantity && decimals && reissuable && script && name && description
         | case _ => throw()
         | }
         |""".stripMargin

    val result = runScript(
      s,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("BurnTransaction binding") {
    val tx = TxHelpers.burn(IssuedAsset(ByteStr.fromLong(1)))
    val result = runScript(
      s"""
         |match tx {
         | case t : BurnTransaction =>
         |   ${provenPart(tx)}
         |   let quantity = t.quantity == ${tx.quantity}
         |   let assetId = t.assetId == base58'${tx.asset.id.toString}'
         |   ${assertProvenPart("t")} && quantity && assetId
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("ReissueTransaction binding") {
    val tx = TxHelpers.reissue(IssuedAsset(ByteStr.fromLong(1)))
    val result = runScript(
      s"""
         |match tx {
         | case t : ReissueTransaction =>
         |   ${provenPart(tx)}
         |   let quantity = t.quantity == ${tx.quantity}
         |   let assetId = t.assetId == base58'${tx.asset.id.toString}'
         |   let reissuable = t.reissuable == ${tx.reissuable}
         |   ${assertProvenPart("t")} && quantity && assetId && reissuable
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("CreateAliasTransaction binding") {
    val tx = TxHelpers.createAlias("alias")
    val result = runScript(
      s"""
         |match tx {
         | case t : CreateAliasTransaction =>
         |   ${provenPart(tx)}
         |   let alias = t.alias == ${Json.toJson(tx.alias.name)}
         |   ${assertProvenPart("t")} && alias
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("LeaseTransaction binding") {
    val tx = TxHelpers.lease()
    val result = runScript(
      s"""
         |match tx {
         | case t : LeaseTransaction =>
         |   ${provenPart(tx)}
         |   let amount = t.amount == ${tx.amount}
         |   let recipient = match (t.recipient) {
         |       case a: Address => a.bytes == base58'${tx.recipient.cast[Address].fold("")(a => Base58.encode(a.bytes))}'
         |       case a: Alias => a.alias == ${Json.toJson(tx.recipient.cast[Alias].fold("")(_.name))}
         |      }
         |   ${assertProvenPart("t")} && amount && recipient
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("LeaseCancelTransaction binding") {
    val tx = TxHelpers.leaseCancel(ByteStr.fill(32)(1))
    val result = runScript(
      s"""
         |match tx {
         | case t : LeaseCancelTransaction =>
         |   ${provenPart(tx)}
         |   let leaseId = t.leaseId == base58'${tx.leaseId.toString}'
         |   ${assertProvenPart("t")} && leaseId
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("SponsorFeeTransaction binding (+ cancel sponsorship transaction)") {
    Seq(
      TxHelpers.sponsor(IssuedAsset(ByteStr.fromLong(1))),
      TxHelpers.sponsor(IssuedAsset(ByteStr.fromLong(1)), None)
    ).foreach { tx =>
      val result = runScript(
        s"""
           |match tx {
           | case t : SponsorFeeTransaction =>
           |   ${provenPart(tx)}
           |   let assetId = t.assetId == base58'${tx.asset.id.toString}'
           |   let minSponsoredAssetFee = if (${tx.minSponsoredAssetFee.isDefined}) then extract(t.minSponsoredAssetFee) == ${tx.minSponsoredAssetFee
          .getOrElse(0)} else isDefined(t.minSponsoredAssetFee) == false
           |   ${assertProvenPart("t")} && assetId && minSponsoredAssetFee
           | case _ => throw()
           | }
           |""".stripMargin,
        Coproduct(tx),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("SetScriptTransaction binding") {
    val tx = TxHelpers.setScript(TxHelpers.defaultSigner, ExprScript(CONST_BOOLEAN(true)).explicitGet())
    val result = runScript(
      s"""
         |match tx {
         | case t : SetScriptTransaction =>
         |   ${provenPart(tx)}
         |   let script = if (${tx.script.isDefined}) then blake2b256(extract(t.script)) == base64'${tx.script
        .map(s => ByteStr(crypto.fastHash(s.bytes().arr)).base64)
        .getOrElse("")}' else isDefined(t.script) == false
         |   ${assertProvenPart("t")} && script
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("UpdateAssetInfoTransaction binding") {
    val tx = TxHelpers.updateAssetInfo(ByteStr.fill(32)(1))
    val scriptSource =
      s"""
         |match tx {
         | case t : UpdateAssetInfoTransaction =>
         |   ${provenPart(tx)}
         |   let name        = t.name.toBytes()        == base58'${ByteStr(tx.name.getBytes)}'
         |   let description = t.description.toBytes() == base58'${ByteStr(tx.description.getBytes())}'
         |   let assetId     = t.assetId               == base58'${tx.assetId.id}'
         |   ${assertProvenPart("t")} && description && name && assetId
         | case _ => throw()
         | }
         |""".stripMargin

    runScript(scriptSource, Coproduct(tx), V4, T) shouldBe evaluated(true)
  }

  property("InvokeScriptTransaction binding") {
    val tx = TxHelpers.invoke(TxHelpers.secondAddress, func = Some("test"), invoker = TxHelpers.defaultSigner, payments = Seq(Payment(1, Waves)))
    val checkArgsScript = if (tx.funcCallOpt.get.args.nonEmpty) {
      tx.funcCallOpt.get.args
        .collect {
          case CONST_LONG(i)    => i.toString
          case CONST_STRING(s)  => s""""$s""""
          case CONST_BOOLEAN(b) => b.toString
          case CONST_BYTESTR(b) => s"base64'${b.base64}'"
        }
        .zipWithIndex
        .map { case (str, i) => s"t.args[$i] == $str" }
        .mkString("let checkArgs = ", " && ", "\n")
    } else {
      "let checkArgs = true"
    }

    val script =
      s"""
         |match tx {
         | case t : InvokeScriptTransaction  =>
         |   ${provenPart(tx)}
         |   let dAppAddressBytes = match t.dApp {
         |     case ad : Address => ad.bytes
         |     case _ : Alias => base58''
         |   }
         |   let dappAddress = dAppAddressBytes == base58'${Base58.encode(tx.dApp.bytes)}'
         |
         |   let paymentAmount = if(${tx.payments.nonEmpty})
         |     then extract(t.payment).amount == ${tx.payments.headOption.map(_.amount).getOrElse(-1)}
         |     else isDefined(t.payment) == false
         |
         |   let paymentAssetId = if(${tx.payments.nonEmpty})
         |     then if (${tx.payments.headOption.exists(_.assetId != Waves)})
         |             then extract(t.payment).assetId == base58'${tx.payments.headOption.flatMap(_.assetId.maybeBase58Repr).getOrElse("")}'
         |             else isDefined(extract(t.payment).assetId) == false
         |     else isDefined(t.payment) == false
         |
         |   let feeAssetId = if (${tx.feeAssetId != Waves})
         |      then extract(t.feeAssetId) == base58'${tx.feeAssetId.maybeBase58Repr.getOrElse("")}'
         |      else isDefined(t.feeAssetId) == false
         |
         |   let checkFunc = t.function == "${tx.funcCallOpt.get.function.funcName}"
         |   $checkArgsScript

         |   ${assertProvenPart("t")} && dappAddress && paymentAmount && paymentAssetId && feeAssetId && checkFunc && checkArgs
         |
         | case _ => throw()
         | }
         |""".stripMargin
    val result = runScriptWithCustomContext(script, tx, V3)
    result shouldBe evaluated(true)
  }

  property("InvokeScriptTransaction V4 context multiple payments") {
    val tx = TxHelpers.invoke(
      dApp = TxHelpers.secondAddress,
      func = Some("test"),
      invoker = TxHelpers.defaultSigner,
      payments = Seq(Payment(1, Waves), Payment(5, IssuedAsset(ByteStr.fill(32)(1))))
    )
    val paymentsStr = tx.payments
      .flatMap(_.assetId.maybeBase58Repr)
      .map(a => s"base58'$a'")
      .reverse
      .mkString("[", ",", "]")

    val size = tx.payments.size
    val script =
      s"""
         | func assetsAmountSum(acc: Int, p: AttachedPayment) = acc + p.amount
         | func extractAssets(acc: List[ByteVector], p: AttachedPayment) =
         |   match p.assetId {
         |     case _: Unit       => acc
         |     case a: ByteVector => a :: acc
         |   }
         |
         | match tx {
         |   case t : InvokeScriptTransaction =>
         |     let paymentAmount = ${Common.fold(size, "t.payments", "0", "assetsAmountSum")()} == ${tx.payments.map(_.amount).sum}
         |     let paymentAssets = ${Common.fold(size, "t.payments", GlobalValNames.Nil, "extractAssets")()} == $paymentsStr
         |
         |     paymentAmount && paymentAssets
         |
         |   case _ => throw()
         | }
         |""".stripMargin

    val blockchain = stub[Blockchain]
    (() => blockchain.settings).when().returning(WavesSettings.default().blockchainSettings)
    (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.BlockV5.id -> 0))
    (() => blockchain.settings).when().returning(WavesSettings.default().blockchainSettings)

    val result = runScriptWithCustomContext(script, tx, V4, blockchain)
    result shouldBe evaluated(true)
  }

  property("InvokeScriptTransaction binding in sync call") {
    def assetVerifier(invoke: InvokeScriptTransaction, masterDApp: KeyPair, serviceDApp: KeyPair, asset: Asset) = TestCompiler(V6).compileAsset(
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |{-# SCRIPT_TYPE ASSET #-}
         |
         |match tx {
         |  case t : InvokeScriptTransaction => 
         |    t.dApp == Address(base58'${serviceDApp.toAddress}') &&
         |      t.payments == [AttachedPayment(base58'$asset', 1)] &&
         |      t.feeAssetId == unit &&
         |      t.function == "bar" &&
         |      t.args == [12345] &&
         |      t.id == base58'${invoke.id()}' &&
         |      t.fee == 0 &&
         |      t.timestamp == ${invoke.timestamp} &&
         |      t.version == 0 &&
         |      t.sender == Address(base58'${invoke.dApp}') &&
         |      t.senderPublicKey == base58'${masterDApp.publicKey}' &&
         |      t.bodyBytes == base58''
         |  case _ => false
         |}
         |
       """.stripMargin
    )

    def masterDAppScript(asset: Asset, serviceDApp: Address) = TestCompiler(V6).compileContract(
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func foo() = {
         |  strict inv = invoke(Address(base58'$serviceDApp'), "bar", [12345], [AttachedPayment(base58'$asset', 1)])
         |  ([], nil)
         |}
         """.stripMargin
    )

    val serviceDAppScript = TestCompiler(V6).compileContract(
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func bar(arg: Int) = {
         |  ([], nil)
         |}
         """.stripMargin
    )

    val invoker     = TxHelpers.signer(1)
    val masterDApp  = TxHelpers.signer(2)
    val serviceDApp = TxHelpers.signer(3)

    withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(invoker, masterDApp, serviceDApp)) { d =>
      val invoke = TxHelpers.invoke(masterDApp.toAddress, func = Some("foo"), invoker = invoker)
      val issue  = TxHelpers.issue(masterDApp, script = Some(TestCompiler(V6).compileAsset("true")))
      val setAssetScript =
        TxHelpers.setAssetScript(masterDApp, issue.asset, assetVerifier(invoke, masterDApp, serviceDApp, issue.asset), fee = 1.waves)

      d.appendBlock(
        issue,
        setAssetScript,
        TxHelpers.setScript(masterDApp, masterDAppScript(issue.asset, serviceDApp.toAddress)),
        TxHelpers.setScript(serviceDApp, serviceDAppScript)
      )
      d.appendAndAssertSucceed(invoke)
    }
  }

  property("InvokeExpressionTransaction binding") {
    val expression = TestCompiler(V6).compileFreeCall("[]")
    def script(tx: InvokeExpressionTransaction) =
      s"""
         | match tx {
         |   case t: InvokeExpressionTransaction  =>
         |     ${provenPart(tx)}
         |     let checkFeeAssetId = t.feeAssetId == ${tx.feeAssetId.fold(GlobalValNames.Unit)(a => s"base58'${a.id.toString}'")}
         |     let checkExpression = t.expression == base58'${expression.bytes()}'
         |     ${assertProvenPart("t")} && checkFeeAssetId && checkExpression
         |   case _ => throw()
         | }
       """.stripMargin

    val fee     = ciFee(freeCall = true).sample.get
    val account = accountGen.sample.get
    val asset   = IssuedAsset(ByteStr.fromBytes(1, 2, 3))
    val tx1     = InvokeExpressionTransaction.selfSigned(TxVersion.V1, account, expression, fee, Waves, Random.nextLong()).explicitGet()
    val tx2     = InvokeExpressionTransaction.selfSigned(TxVersion.V1, account, expression, fee, asset, Random.nextLong()).explicitGet()

    runScriptWithCustomContext(script(tx1), tx1, V6) shouldBe evaluated(true)
    runScriptWithCustomContext(script(tx2), tx2, V6) shouldBe evaluated(true)
  }

  property("SetAssetScriptTransaction binding") {
    val tx = TxHelpers.setAssetScript(TxHelpers.defaultSigner, IssuedAsset(ByteStr.fill(32)(1)), ExprScript(CONST_BOOLEAN(true)).explicitGet())
    val result = runScript(
      s"""
         |match tx {
         | case t : SetAssetScriptTransaction =>
         |   ${provenPart(tx)}
         |   let script = if (${tx.script.isDefined}) then extract(t.script) == base64'${tx.script
        .map(_.bytes().base64)
        .getOrElse("")}' else isDefined(t.script) == false
         |    let assetId = t.assetId == base58'${tx.asset.id.toString}'
         |   ${assertProvenPart("t")} && script && assetId
         | case _ => throw()
         | }
         |""".stripMargin,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("DataTransaction binding") {
    for {
      version    <- DirectiveDictionary[StdLibVersion].all
      useV4Check <- Seq(true, false)
      entryCount <- Seq(0, 1, DataTransaction.MaxEntryCount)
      dataEntries = (1 to entryCount).map(idx => StringDataEntry(s"key$idx", "value"))
      tx <- Seq(TxHelpers.data(TxHelpers.defaultSigner, dataEntries), TxHelpers.dataV2(TxHelpers.defaultSigner, dataEntries))
    } {
      def check(i: Int, rideType: String, valueOpt: Option[Any]): String = {
        val key        = s""" "${tx.data(i).key}" """
        val keyCheck   = s"t.data[$i].key == $key"
        val valueCheck = valueOpt.map(value => s"t.data[$i].value == $value").getOrElse("true")
        val matchCheck =
          s"""
             | match t.data[$i] {
             |   case entry: $rideType =>
             |     entry.key == $key &&
             |     ${valueOpt.map(value => s"entry.value == $value").getOrElse("true")}
             |   case _ =>
             |     throw("unexpected type instead $rideType")
             | }
         """.stripMargin

        if (useV4Check)
          s"$keyCheck && $matchCheck"
        else
          s"$keyCheck && $valueCheck"
      }

      def toRideChecks(i: Int): String =
        tx.data(i) match {
          case e: IntegerDataEntry => check(i, FieldNames.IntegerEntry, Some(e.value))
          case e: BooleanDataEntry => check(i, FieldNames.BooleanEntry, Some(e.value))
          case e: BinaryDataEntry =>
            check(
              i,
              FieldNames.BinaryEntry,
              Some(
                if (e.value.isEmpty)
                  "base58''"
                else
                  e.value.arr
                    .sliding(Global.MaxLiteralLength / 2, Global.MaxLiteralLength / 2)
                    .map(arr => s"base58'${Base58.encode(arr)}'")
                    .mkString(" + ")
              )
            )
          case e: StringDataEntry => check(i, FieldNames.StringEntry, Some(s""" "${e.value}" """))
          case _: EmptyDataEntry  => check(i, FieldNames.DeleteEntry, None)
        }

      val assertTxData =
        if (tx.data.nonEmpty)
          tx.data.indices
            .map(toRideChecks)
            .mkString(" && ")
        else
          "true"

      val script =
        s"""
           | match tx {
           |   case t : DataTransaction =>
           |     ${provenPart(tx)}
           |     ${assertProvenPart("t")} &&
           |     $assertTxData
           |   case _ => throw()
           | }
          """.stripMargin

      val result = runScript(script, Coproduct(tx), ctxV = version, chainId = T)

      if (useV4Check && version < V4 && tx.data.nonEmpty)
        result should produce("Compilation failed: Undefined type")
      else if (!useV4Check && version >= V4 && tx.data.exists(!_.isInstanceOf[EmptyDataEntry]))
        result should produce("Undefined field `value` of variable")
      else
        result shouldBe evaluated(true)
    }
  }

  property("MassTransferTransaction binding") {
    val tx = TxHelpers.massTransfer()
    def pg(i: Int) =
      s"""let recipient$i = match (t.transfers[$i].recipient) {
         |case a: Address => a.bytes == base58'${tx.transfers(i).address.cast[Address].fold("")(a => Base58.encode(a.bytes))}'
         |case a: Alias => a.alias == ${Json.toJson(tx.transfers(i).address.cast[Alias].map(_.name).getOrElse(""))}
         |}
         |let amount$i = t.transfers[$i].amount == ${tx.transfers(i).amount}
       """.stripMargin

    val resString =
      if (tx.transfers.isEmpty) assertProvenPart("t")
      else
        assertProvenPart("t") + s" &&" + {
          tx.transfers.indices
            .map(i => s"recipient$i && amount$i")
            .mkString(" && ")
        }

    val script = s"""
                    |match tx {
                    | case t : MassTransferTransaction =>
                    |    let assetId = if (${tx.assetId != Waves}) then extract(t.assetId) == base58'${tx.assetId.maybeBase58Repr
      .getOrElse("")}'
                    |      else isDefined(t.assetId) == false
                    |     let transferCount = t.transferCount == ${tx.transfers.length}
                    |     let totalAmount = t.totalAmount == ${tx.transfers.map(_.amount.value).sum}
                    |     let attachment = t.attachment == base58'${Base58.encode(tx.attachment.arr)}'
                    |     ${tx.transfers.indices.map(pg).mkString("\n")}
                    |   ${provenPart(tx)}
                    |   $resString && assetId && transferCount && totalAmount && attachment
                    | case _ => throw()
                    | }
                    |""".stripMargin

    val result = runScript(
      script,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("ExchangeTransaction binding") {
    val tx = TxHelpers.exchangeFromOrders(
      TxHelpers.orderV3(OrderType.BUY, IssuedAsset(ByteStr.fill(32)(1))),
      TxHelpers.orderV3(OrderType.SELL, IssuedAsset(ByteStr.fill(32)(1)))
    )
    def pg(ord: Order) = {
      val oType = ord.orderType.toString
      val script = s"""
                      |   let ${oType}Id = t.${oType}Order.id == base58'${ord.idStr()}'
                      |   let ${oType}Sender = t.${oType}Order.sender == addressFromPublicKey(base58'${ord.sender}')
                      |   let ${oType}SenderPk = t.${oType}Order.senderPublicKey == base58'${ord.sender}'
                      |   let ${oType}MatcherPk = t.${oType}Order.matcherPublicKey == base58'${ord.matcherPublicKey}'
                      |   let ${oType}Price = t.${oType}Order.price == ${ord.price}
                      |   let ${oType}Amount = t.${oType}Order.amount == ${ord.amount}
                      |   let ${oType}Timestamp = t.${oType}Order.timestamp == ${ord.timestamp}
                      |   let ${oType}Expiration = t.${oType}Order.expiration == ${ord.expiration}
                      |   let ${oType}OrderMatcherFee = t.${oType}Order.matcherFee == ${ord.matcherFee}
                      |   let ${oType}BodyBytes = t.${oType}Order.bodyBytes == base58'${Base58.encode(ord.bodyBytes())}'
                      |   ${Range(0, 8).map(letProof(Proofs(Seq(ord.signature)), s"t.${oType}Order")).mkString("\n")}
                      |   let ${oType}Proofs =${assertProofs(s"t.${oType}Order")}
                      |   let ${oType}AssetPairAmount = if (${ord.assetPair.amountAsset != Waves}) then extract(t.${oType}Order.assetPair.amountAsset) == base58'${ord.assetPair.amountAsset.maybeBase58Repr
        .getOrElse("")}'
                      |   else isDefined(t.${oType}Order.assetPair.amountAsset) == false
                      |   let ${oType}AssetPairPrice = if (${ord.assetPair.priceAsset != Waves}) then extract(t.${oType}Order.assetPair.priceAsset) == base58'${ord.assetPair.priceAsset.maybeBase58Repr
        .getOrElse("")}'
                      |   else isDefined(t.${oType}Order.assetPair.priceAsset) == false
                      |   let ${oType}MatcherFeeAssetId = if (${ord.matcherFeeAssetId != Waves}) then extract(t.${oType}Order.matcherFeeAssetId) == base58'${ord.matcherFeeAssetId.maybeBase58Repr
        .getOrElse("")}'
                      |   else isDefined(t.${oType}Order.matcherFeeAssetId) == false
       """.stripMargin

      val lets = List(
        "Id",
        "Sender",
        "SenderPk",
        "MatcherPk",
        "Price",
        "Amount",
        "Timestamp",
        "Expiration",
        "OrderMatcherFee",
        "BodyBytes",
        "AssetPairAmount",
        "AssetPairPrice",
        "Proofs",
        "MatcherFeeAssetId"
      ).map(i => s"$oType$i")
        .mkString(" && ")

      (script, lets)
    }

    val s = s"""|match tx {
                | case t : ExchangeTransaction =>
                |   ${provenPart(tx)}
                |   let price = t.price == ${tx.price}
                |   let amount = t.amount == ${tx.amount}
                |   let buyMatcherFee = t.buyMatcherFee == ${tx.buyMatcherFee}
                |   let sellMatcherFee = t.sellMatcherFee == ${tx.sellMatcherFee}
                |   ${pg(tx.buyOrder)._1}
                |   ${pg(tx.sellOrder)._1}
                |   ${assertProvenPart("t")} && price && amount && buyMatcherFee && sellMatcherFee && ${pg(tx.buyOrder)._2} && ${pg(tx.sellOrder)._2}
                | case _ => throw()
                | }
                |""".stripMargin

    val result = runScript(
      s,
      Coproduct(tx),
      T
    )
    result shouldBe evaluated(true)
  }

  property("Order binding") {
    val order = TxHelpers.orderV3(OrderType.BUY, IssuedAsset(ByteStr.fill(32)(1)))
    val s = s"""
               |match tx {
               | case t : Order =>
               |   let id = t.id == base58'${order.id().toString}'
               |   let sender = t.sender == addressFromPublicKey(base58'${order.sender}')
               |   let senderPublicKey = t.senderPublicKey == base58'${order.sender}'
               |   let matcherPublicKey = t.matcherPublicKey == base58'${order.matcherPublicKey}'
               |   let timestamp = t.timestamp == ${order.timestamp}
               |   let price = t.price == ${order.price}
               |   let amount = t.amount == ${order.amount}
               |   let expiration = t.expiration == ${order.expiration}
               |   let matcherFee = t.matcherFee == ${order.matcherFee}
               |   let bodyBytes = t.bodyBytes == base64'${ByteStr(order.bodyBytes.apply()).base64}'
               |   ${Range(0, 8).map(letProof(order.proofs, "t")).mkString("\n")}
               |   let assetPairAmount = if (${order.assetPair.amountAsset != Waves}) then extract(t.assetPair.amountAsset) == base58'${order.assetPair.amountAsset.maybeBase58Repr
      .getOrElse("")}'
               |   else isDefined(t.assetPair.amountAsset) == false
               |   let assetPairPrice = if (${order.assetPair.priceAsset != Waves}) then extract(t.assetPair.priceAsset) == base58'${order.assetPair.priceAsset.maybeBase58Repr
      .getOrElse("")}'
               |   else isDefined(t.assetPair.priceAsset) == false
               |   let matcherFeeAssetId = if (${order.matcherFeeAssetId != Waves}) then extract(t.matcherFeeAssetId) == base58'${order.matcherFeeAssetId.maybeBase58Repr
      .getOrElse("")}'
               |   else isDefined(t.matcherFeeAssetId) == false
               |   id && sender && senderPublicKey && matcherPublicKey && timestamp && price && amount && expiration && matcherFee && bodyBytes && ${assertProofs(
      "t"
    )} && assetPairAmount && assetPairPrice && matcherFeeAssetId
               | case _ => throw()
               | }
               |""".stripMargin

    val result = runScript(
      s,
      Coproduct[In](order),
      T
    )
    result shouldBe evaluated(true)
  }

  property("Order type bindings") {
    val orders = Seq(
      TxHelpers.orderV3(OrderType.BUY, IssuedAsset(ByteStr.fill(32)(1))),
      TxHelpers.orderV3(OrderType.SELL, IssuedAsset(ByteStr.fill(32)(1)))
    )
    orders.foreach { ord =>
      val src =
        s"""
           |match tx {
           |  case o: Order =>
           |    let orderType = o.orderType
           |    orderType == ${if (ord.orderType == OrderType.BUY) GlobalValNames.Buy else GlobalValNames.Sell}
           |  case _ => throw()
           |}
       """.stripMargin

      runScript(src, Coproduct[In](ord), T) shouldBe an[Left[?, ?]]
      runWithSmartTradingActivated(src, Coproduct[In](ord), 'T') shouldBe evaluated(true)
    }
  }

  property("Bindings w/wo proofs/order") {
    val assetSupportedTxTypes: Seq[String] =
      Seq(
        "TransferTransaction",
        "ReissueTransaction",
        "BurnTransaction",
        "SetAssetScriptTransaction"
      )
    val order = TxHelpers.orderV3(OrderType.BUY, IssuedAsset(ByteStr.fill(32)(1)))
    assetSupportedTxTypes.foreach { txType =>
      val src1 =
        s"""
           |let expectedProof = base58'satoshi'
           |match tx {
           |  case t: $txType => t.proofs[1] == expectedProof
           |  case _ => true
           |}
        """.stripMargin

      val src2 =
        s"""
           |match tx {
           |  case _: Order => 1
           |  case _: $txType => 2
           |  case _ => 3
           |}
         """.stripMargin

      val noProofsError = s"Undefined field `proofs` of variable of type `$txType`"

      runForAsset(src1) should produce(noProofsError)

      runForAsset(src2).left.value

      runScript[EVALUATED](src1, Coproduct[In](order)) shouldBe Right(CONST_BOOLEAN(true))
      runScript[EVALUATED](src2, Coproduct[In](order)) shouldBe Right(CONST_LONG(1))
    }
  }

  property(
    s"NODE-1039. Orders should contain attachment field in Ride version >= V8 after ${BlockchainFeatures.LightNode} activation"
  ) {
    val issuer  = TxHelpers.signer(1)
    val buyer   = TxHelpers.signer(2)
    val matcher = TxHelpers.signer(3)

    val attachment = ByteStr.fill(32)(1)
    val exchangeTxScript =
      s"""
         |match tx {
         |  case o: ExchangeTransaction =>
         |    let orderWithAttachment = o.buyOrder
         |    let orderWithoutAttachment = o.sellOrder
         |    orderWithAttachment.attachment == base58'$attachment' && orderWithoutAttachment.attachment == unit
         |  case _ => true
         |}
       """.stripMargin

    val buyOrderScript =
      s"""
         |match tx {
         |  case o: Order =>
         |    o.attachment == base58'$attachment'
         |  case _ => true
         |}
         |""".stripMargin

    val sellOrderScript =
      s"""
         |match tx {
         |  case o: Order =>
         |    o.attachment == unit
         |  case _ => true
         |}
         |""".stripMargin

    val issue = TxHelpers.issue(issuer)
    val orderWithAttachment =
      TxHelpers.order(OrderType.BUY, Waves, issue.asset, version = Order.V4, sender = buyer, matcher = matcher, attachment = Some(attachment))
    val exchange = () =>
      TxHelpers.exchangeFromOrders(
        orderWithAttachment,
        TxHelpers.order(OrderType.SELL, Waves, issue.asset, version = Order.V4, sender = issuer, matcher = matcher),
        matcher = matcher,
        version = TxVersion.V3
      )

    Seq(exchangeTxScript -> matcher, buyOrderScript -> buyer, sellOrderScript -> issuer).foreach { case (script, smartAcc) =>
      val compiledScript = TestCompiler(V8).compileExpression(script)

      Seq(V4, V5, V6, V7).foreach { v =>
        TestCompiler(v).compileExpressionE(script) should produce("Undefined field `attachment` of variable of type `Order`")

        withDomain(
          DomainPresets.BlockRewardDistribution.setFeaturesHeight(BlockchainFeatures.LightNode -> Int.MaxValue),
          AddrWithBalance.enoughBalances(issuer, matcher, buyer)
        ) { d =>
          d.appendBlock(issue)

          d.appendBlock(TxHelpers.setScript(smartAcc, compiledScript.asInstanceOf[ExprScriptImpl].copy(stdLibVersion = v)))
          d.appendBlockE(exchange()) should produce("key not found: attachment")
        }
      }

      withDomain(
        DomainPresets.BlockRewardDistribution.setFeaturesHeight(BlockchainFeatures.LightNode -> 4),
        AddrWithBalance.enoughBalances(issuer, matcher, buyer)
      ) { d =>
        d.appendBlock(issue)

        d.appendBlockE(TxHelpers.setScript(smartAcc, compiledScript)) should produce("Light Node feature has not been activated yet")
        d.appendBlock()
        d.appendBlockE(TxHelpers.setScript(smartAcc, compiledScript)) should beRight

        d.appendBlockE(exchange()) should beRight
      }
    }
  }

  def runForAsset(script: String): Either[String, EVALUATED] = {
    import cats.syntax.monoid.*
    import com.wavesplatform.lang.v1.CTX.*

    val expr       = Parser.parseExpr(script).get.value
    val directives = DirectiveSet(V2, AssetType, Expression).explicitGet()
    val ctx =
      PureContext.build(V2, useNewPowPrecision = true).withEnvironment[Environment] |+|
        CryptoContext.build(Global, V2).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V2, AssetType, Expression).explicitGet(), fixBigScriptField = true)

    val environment = new WavesEnvironment(
      chainId,
      Coeval(???),
      null,
      EmptyBlockchain,
      Coproduct[Environment.Tthis](Environment.AssetId(Array())),
      directives,
      ByteStr.empty
    )
    for {
      compileResult <- compiler.ExpressionCompiler(ctx.compilerContext, expr)
      (typedExpr, _) = compileResult
      r <- EvaluatorV1().apply[EVALUATED](ctx.evaluationContext(environment), typedExpr).leftMap(_.message)
    } yield r
  }

  def runWithSmartTradingActivated(script: String, t: In = null, chainId: Byte = chainId): Either[String, EVALUATED] = {
    import cats.syntax.monoid.*
    import com.wavesplatform.lang.v1.CTX.*

    val expr = Parser.parseExpr(script).get.value

    val directives = DirectiveSet(V2, Account, Expression).explicitGet()
    val blockchain = stub[Blockchain]
    (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.BlockV5.id -> 0))

    val ctx =
      PureContext.build(V2, useNewPowPrecision = true).withEnvironment[Environment] |+|
        CryptoContext.build(Global, V2).withEnvironment[Environment] |+|
        WavesContext.build(Global, directives, fixBigScriptField = true)

    val env = new WavesEnvironment(
      chainId,
      Coeval(buildThisValue(t, blockchain, directives, Coproduct[Environment.Tthis](Environment.AssetId(Array()))).explicitGet()),
      null,
      EmptyBlockchain,
      Coproduct[Environment.Tthis](Environment.AssetId(Array())),
      directives,
      ByteStr.empty
    )

    for {
      compileResult <- ExpressionCompiler(ctx.compilerContext, expr)
      (typedExpr, _) = compileResult
      r <- EvaluatorV1().apply[EVALUATED](ctx.evaluationContext(env), typedExpr).leftMap(_.message)
    } yield r
  }
}
