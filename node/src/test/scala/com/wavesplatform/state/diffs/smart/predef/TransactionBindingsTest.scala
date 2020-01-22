package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.lang.Testing.evaluated
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.{Proofs, ProvenTransaction, VersionedTransaction}
import com.wavesplatform.utils.EmptyBlockchain
import com.wavesplatform.{NoShrink, TransactionGen, crypto}
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json
import shapeless.Coproduct

class TransactionBindingsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val T = 'T'.toByte

  def letProof(p: Proofs, prefix: String)(i: Int) =
    s"let ${prefix.replace(".", "")}proof$i = $prefix.proofs[$i] == base58'${p.proofs.applyOrElse(i, (_: Int) => ByteStr.empty).base58}'"

  def provenPart(t: ProvenTransaction): String = {
    val version = t match {
      case v: VersionedTransaction => v.version
      case _                       => 1
    }
    s"""
       |   let id = t.id == base58'${t.id().base58}'
       |   let fee = t.fee == ${t.assetFee._2}
       |   let timestamp = t.timestamp == ${t.timestamp}
       |   let bodyBytes = blake2b256(t.bodyBytes) == base64'${ByteStr(crypto.fastHash(t.bodyBytes.apply().array)).base64}'
       |   let sender = t.sender == addressFromPublicKey(base58'${ByteStr(t.sender).base58}')
       |   let senderPublicKey = t.senderPublicKey == base58'${ByteStr(t.sender).base58}'
       |   let version = t.version == $version
       |   ${Range(0, 8).map(letProof(t.proofs, "t")).mkString("\n")}
     """.stripMargin
  }

  def assertProofs(p: String): String = {
    val prefix = p.replace(".", "")
    s"${prefix}proof0 && ${prefix}proof1 && ${prefix}proof2 && ${prefix}proof3 && ${prefix}proof4 && ${prefix}proof5 && ${prefix}proof6 && ${prefix}proof7"
  }
  def assertProvenPart(prefix: String) =
    s"id && fee && timestamp && sender && senderPublicKey && ${assertProofs(prefix)} && bodyBytes && version"

  property("TransferTransaction binding") {
    forAll(Gen.oneOf(transferV1Gen, transferV2Gen)) { t =>
      // `version`  is not properly bound yet
      val result = runScript(
        s"""
           |match tx {
           | case t : TransferTransaction  =>
           |   ${provenPart(t)}
           |   let amount = t.amount == ${t.amount}
           |   let feeAssetId = if (${t.feeAssetId != Waves})
           |      then extract(t.feeAssetId) == base58'${t.feeAssetId.maybeBase58Repr.getOrElse("")}'
           |      else isDefined(t.feeAssetId) == false
           |   let assetId = if (${t.assetId != Waves})
           |      then extract(t.assetId) == base58'${t.assetId.maybeBase58Repr.getOrElse("")}'
           |      else isDefined(t.assetId) == false
           |   let recipient = match (t.recipient) {
           |       case a: Address => a.bytes == base58'${t.recipient.cast[Address].map(_.bytes.base58).getOrElse("")}'
           |       case a: Alias => a.alias == ${Json.toJson(t.recipient.cast[Alias].map(_.name).getOrElse(""))}
           |      }
           |    let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
           |   ${assertProvenPart("t")} && amount && feeAssetId && assetId && recipient && attachment
           | case other => throw()
           | }
           |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("IssueTransaction binding") {
    forAll(issueGen) { t =>
      val s = s"""
                 |match tx {
                 | case t : IssueTransaction =>
                 |   ${provenPart(t)}
                 |   let quantity = t.quantity == ${t.quantity}
                 |   let decimals = t.decimals == ${t.decimals}
                 |   let reissuable = t.reissuable == ${t.reissuable}
                 |   let name = t.name == base58'${ByteStr(t.name).base58}'
                 |   let description = t.description == base58'${ByteStr(t.description).base58}'
                 |   let script = if (${t.script.isDefined}) then extract(t.script) == base64'${t.script
                   .map(_.bytes().base64)
                   .getOrElse("")}' else isDefined(t.script) == false
                 |   ${assertProvenPart("t")} && quantity && decimals && reissuable && script && name && description
                 | case other => throw()
                 | }
                 |""".stripMargin

      val result = runScript(
        s,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("BurnTransaction binding") {
    forAll(burnGen) { t =>
      val result = runScript(
        s"""
          |match tx {
          | case t : BurnTransaction =>
          |   ${provenPart(t)}
          |   let quantity = t.quantity == ${t.quantity}
          |   let assetId = t.assetId == base58'${t.asset.id.base58}'
          |   ${assertProvenPart("t")} && quantity && assetId
          | case other => throw()
          | }
          |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("ReissueTransaction binding") {
    forAll(reissueGen) { t =>
      val result = runScript(
        s"""
          |match tx {
          | case t : ReissueTransaction =>
          |   ${provenPart(t)}
          |   let quantity = t.quantity == ${t.quantity}
          |   let assetId = t.assetId == base58'${t.asset.id.base58}'
          |   let reissuable = t.reissuable == ${t.reissuable}
          |   ${assertProvenPart("t")} && quantity && assetId && reissuable
          | case other => throw()
          | }
          |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("CreateAliasTransaction binding") {
    forAll(createAliasGen) { t =>
      val result = runScript(
        s"""
          |match tx {
          | case t : CreateAliasTransaction =>
          |   ${provenPart(t)}
          |   let alias = t.alias == ${Json.toJson(t.alias.name)}
          |   ${assertProvenPart("t")} && alias
          | case other => throw()
          | }
          |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("LeaseTransaction binding") {
    forAll(leaseGen) { t =>
      val result = runScript(
        s"""
          |match tx {
          | case t : LeaseTransaction =>
          |   ${provenPart(t)}
          |   let amount = t.amount == ${t.amount}
          |   let recipient = match (t.recipient) {
          |       case a: Address => a.bytes == base58'${t.recipient.cast[Address].map(_.bytes.base58).getOrElse("")}'
          |       case a: Alias => a.alias == ${Json.toJson(t.recipient.cast[Alias].map(_.name).getOrElse(""))}
          |      }
          |   ${assertProvenPart("t")} && amount && recipient
          | case other => throw()
          | }
          |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("LeaseCancelTransaction binding") {
    forAll(leaseCancelGen) { t =>
      val result = runScript(
        s"""
          |match tx {
          | case t : LeaseCancelTransaction =>
          |   ${provenPart(t)}
          |   let leaseId = t.leaseId == base58'${t.leaseId.base58}'
          |   ${assertProvenPart("t")} && leaseId
          | case other => throw()
          | }
          |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("SponsorFeeTransaction binding (+ cancel sponsorship transaction)") {
    forAll(Gen.oneOf(sponsorFeeGen, cancelFeeSponsorshipGen)) { t =>
      val result = runScript(
        s"""
          |match tx {
          | case t : SponsorFeeTransaction =>
          |   ${provenPart(t)}
          |   let assetId = t.assetId == base58'${t.asset.id.base58}'
          |   let minSponsoredAssetFee = if (${t.minSponsoredAssetFee.isDefined}) then extract(t.minSponsoredAssetFee) == ${t.minSponsoredAssetFee
             .getOrElse(0)} else isDefined(t.minSponsoredAssetFee) == false
          |   ${assertProvenPart("t")} && assetId && minSponsoredAssetFee
          | case other => throw()
          | }
          |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("SetScriptTransaction binding") {
    forAll(setScriptTransactionGen) { t =>
      val result = runScript(
        s"""
           |match tx {
           | case t : SetScriptTransaction =>
           |   ${provenPart(t)}
           |   let script = if (${t.script.isDefined}) then blake2b256(extract(t.script)) == base64'${t.script
             .map(s => ByteStr(crypto.fastHash(s.bytes().arr)).base64)
             .getOrElse("")}' else isDefined(t.script) == false
           |   ${assertProvenPart("t")} && script
           | case other => throw()
           | }
           |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("InvokeScriptTransaction binding") {
    forAll(invokeScriptGen) { t =>
      val checkArgsScript = if (t.funcCallOpt.get.args.nonEmpty) {
        t.funcCallOpt.get.args
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
            |   ${provenPart(t)}
            |   let dAppAddressBytes = match t.dApp {
            |     case ad : Address => ad.bytes
            |     case al : Alias => base58''
            |   }
            |   let dappAddress = dAppAddressBytes == base58'${t.dAppAddressOrAlias.bytes.base58}'
            |
            |   let paymentAmount = if(${t.payment.nonEmpty})
            |     then extract(t.payment).amount == ${t.payment.headOption.map(_.amount).getOrElse(-1)}
            |     else isDefined(t.payment) == false
            |
            |   let paymentAssetId = if(${t.payment.nonEmpty})
            |     then if (${t.payment.headOption.exists(_.assetId != Waves)})
            |             then extract(t.payment).assetId == base58'${t.payment.headOption.flatMap(_.assetId.maybeBase58Repr).getOrElse("")}'
            |             else isDefined(extract(t.payment).assetId) == false
            |     else isDefined(t.payment) == false
            |
            |   let feeAssetId = if (${t.feeAssetId != Waves})
            |      then extract(t.feeAssetId) == base58'${t.feeAssetId.maybeBase58Repr.getOrElse("")}'
            |      else isDefined(t.feeAssetId) == false
            |
            |   let checkFunc = t.function == "${t.funcCallOpt.get.function.funcName}"
            |   $checkArgsScript

            |   ${assertProvenPart("t")} && dappAddress && paymentAmount && paymentAssetId && feeAssetId && checkFunc && checkArgs
            |
            | case other => throw()
            | }
            |""".stripMargin
      val result = runScriptWithCustomContext(script, Coproduct(t), T, V3)
      result shouldBe evaluated(true)
    }
  }

  property("SetAssetScriptTransaction binding") {
    forAll(setAssetScriptTransactionGen.map(_._2)) { t =>
      val result = runScript(
        s"""
           |match tx {
           | case t : SetAssetScriptTransaction =>
           |   ${provenPart(t)}
           |   let script = if (${t.script.isDefined}) then extract(t.script) == base64'${t.script
             .map(_.bytes().base64)
             .getOrElse("")}' else isDefined(t.script) == false
           |    let assetId = t.assetId == base58'${t.asset.id.base58}'
           |   ${assertProvenPart("t")} && script && assetId
           | case other => throw() 
           | }
           |""".stripMargin,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("DataTransaction binding") {
    forAll(dataTransactionGen(10, useForScript = true)) { t =>
      def pg(i: Int) = {
        val v = t.data(i) match {
          case e: IntegerDataEntry => e.value.toString
          case e: BooleanDataEntry => e.value.toString
          case e: BinaryDataEntry  => s"base64'${e.value.base64}'"
          case e: StringDataEntry  => Json.toJson(e.value)
        }

        s"""let key$i = t.data[$i].key == ${Json.toJson(t.data(i).key)}
           |let value$i = t.data[$i].value == $v
         """.stripMargin
      }

      val resString =
        if (t.data.isEmpty) assertProvenPart("t") else assertProvenPart("t") + s" && ${t.data.indices.map(i => s"key$i && value$i").mkString(" && ")}"

      val s = s"""
                 |match tx {
                 | case t : DataTransaction =>
                 |   ${provenPart(t)}
                 |   ${t.data.indices.map(pg).mkString("\n")}
                 |   $resString
                 | case other => throw()
                 | }
                 |""".stripMargin

      val result = runScript(
        s,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("MassTransferTransaction binding") {
    forAll(massTransferGen) { t =>
      def pg(i: Int) =
        s"""let recipient$i = match (t.transfers[$i].recipient) {
           |case a: Address => a.bytes == base58'${t.transfers(i).address.cast[Address].map(_.bytes.base58).getOrElse("")}'
           |case a: Alias => a.alias == ${Json.toJson(t.transfers(i).address.cast[Alias].map(_.name).getOrElse(""))}
           |}
           |let amount$i = t.transfers[$i].amount == ${t.transfers(i).amount}
         """.stripMargin

      val resString =
        if (t.transfers.isEmpty) assertProvenPart("t")
        else
          assertProvenPart("t") + s" &&" + {
            t.transfers.indices
              .map(i => s"recipient$i && amount$i")
              .mkString(" && ")
          }

      val script = s"""
                      |match tx {
                      | case t : MassTransferTransaction =>
                      |    let assetId = if (${t.assetId != Waves}) then extract(t.assetId) == base58'${t.assetId.maybeBase58Repr
                        .getOrElse("")}'
                      |      else isDefined(t.assetId) == false
                      |     let transferCount = t.transferCount == ${t.transfers.length}
                      |     let totalAmount = t.totalAmount == ${t.transfers.map(_.amount).sum}
                      |     let attachment = t.attachment == base58'${ByteStr(t.attachment).base58}'
                      |     ${t.transfers.indices.map(pg).mkString("\n")}
                      |   ${provenPart(t)}
                      |   $resString && assetId && transferCount && totalAmount && attachment
                      | case other => throw()
                      | }
                      |""".stripMargin

      val result = runScript(
        script,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("ExchangeTransaction binding") {
    forAll(exchangeTransactionGen) { t =>
      def pg(ord: Order) = {
        val oType = ord.orderType.toString
        val script = s"""
           |   let ${oType}Id = t.${oType}Order.id == base58'${ord.idStr()}'
           |   let ${oType}Sender = t.${oType}Order.sender == addressFromPublicKey(base58'${ByteStr(ord.sender).base58}')
           |   let ${oType}SenderPk = t.${oType}Order.senderPublicKey == base58'${ByteStr(ord.sender).base58}'
           |   let ${oType}MatcherPk = t.${oType}Order.matcherPublicKey == base58'${ByteStr(ord.matcherPublicKey).base58}'
           |   let ${oType}Price = t.${oType}Order.price == ${ord.price}
           |   let ${oType}Amount = t.${oType}Order.amount == ${ord.amount}
           |   let ${oType}Timestamp = t.${oType}Order.timestamp == ${ord.timestamp}
           |   let ${oType}Expiration = t.${oType}Order.expiration == ${ord.expiration}
           |   let ${oType}OrderMatcherFee = t.${oType}Order.matcherFee == ${ord.matcherFee}
           |   let ${oType}BodyBytes = t.${oType}Order.bodyBytes == base58'${ByteStr(ord.bodyBytes()).base58}'
           |   ${Range(0, 8).map(letProof(Proofs(Seq(ByteStr(ord.signature))), s"t.${oType}Order")).mkString("\n")}
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
                |   ${provenPart(t)}
                |   let price = t.price == ${t.price}
                |   let amount = t.amount == ${t.amount}
                |   let buyMatcherFee = t.buyMatcherFee == ${t.buyMatcherFee}
                |   let sellMatcherFee = t.sellMatcherFee == ${t.sellMatcherFee} 
                |   ${pg(t.buyOrder)._1}
                |   ${pg(t.sellOrder)._1}
                |   ${assertProvenPart("t")} && price && amount && buyMatcherFee && sellMatcherFee && ${pg(t.buyOrder)._2} && ${pg(t.sellOrder)._2}
                | case other => throw()
                | }
                |""".stripMargin

      val result = runScript(
        s,
        Coproduct(t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("Order binding") {
    forAll(orderGen) { t =>
      val s = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58'${t.id().base58}'
                 |   let sender = t.sender == addressFromPublicKey(base58'${ByteStr(t.sender).base58}')
                 |   let senderPublicKey = t.senderPublicKey == base58'${ByteStr(t.sender).base58}'
                 |   let matcherPublicKey = t.matcherPublicKey == base58'${ByteStr(t.matcherPublicKey).base58}'
                 |   let timestamp = t.timestamp == ${t.timestamp}
                 |   let price = t.price == ${t.price}
                 |   let amount = t.amount == ${t.amount}
                 |   let expiration = t.expiration == ${t.expiration}
                 |   let matcherFee = t.matcherFee == ${t.matcherFee}
                 |   let bodyBytes = t.bodyBytes == base64'${ByteStr(t.bodyBytes.apply()).base64}'
                 |   ${Range(0, 8).map(letProof(t.proofs, "t")).mkString("\n")}
                 |   let assetPairAmount = if (${t.assetPair.amountAsset != Waves}) then extract(t.assetPair.amountAsset) == base58'${t.assetPair.amountAsset.maybeBase58Repr
                   .getOrElse("")}'
                 |   else isDefined(t.assetPair.amountAsset) == false
                 |   let assetPairPrice = if (${t.assetPair.priceAsset != Waves}) then extract(t.assetPair.priceAsset) == base58'${t.assetPair.priceAsset.maybeBase58Repr
                   .getOrElse("")}'
                 |   else isDefined(t.assetPair.priceAsset) == false
                 |   let matcherFeeAssetId = if (${t.matcherFeeAssetId != Waves}) then extract(t.matcherFeeAssetId) == base58'${t.matcherFeeAssetId.maybeBase58Repr
                   .getOrElse("")}'
                 |   else isDefined(t.matcherFeeAssetId) == false
                 |   id && sender && senderPublicKey && matcherPublicKey && timestamp && price && amount && expiration && matcherFee && bodyBytes && ${assertProofs(
                   "t")} && assetPairAmount && assetPairPrice && matcherFeeAssetId
                 | case other => throw()
                 | }
                 |""".stripMargin

      val result = runScript(
        s,
        Coproduct[In](t),
        T
      )
      result shouldBe evaluated(true)
    }
  }

  property("Order type bindings") {
    forAll(orderGen) { ord =>
      val src =
        s"""
           |match tx {
           |  case o: Order =>
           |    let orderType = o.orderType
           |    orderType == ${if (ord.orderType == OrderType.BUY) "Buy" else "Sell"}
           |  case _ => throw()
           |}
       """.stripMargin

      runScript(src, Coproduct[In](ord), T) shouldBe an[Left[_, _]]
      runWithSmartTradingActivated(src, Coproduct[In](ord), 'T') shouldBe evaluated(true)
    }
  }

  property("Bindings w/wo proofs/order") {
    val assetSupportedTxTypeGen: Gen[String] = Gen.oneOf(
      List(
        "TransferTransaction",
        "ReissueTransaction",
        "BurnTransaction",
        "SetAssetScriptTransaction"
      )
    )

    forAll(assetSupportedTxTypeGen, orderGen) { (txType, in) =>
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
           |  case o: Order => 1
           |  case t: $txType => 2
           |  case _ => 3
           |}
         """.stripMargin

      val noProofsError = s"Compilation failed: Undefined field `proofs` of variable of type `$txType`"

      runForAsset(src1) should produce(noProofsError)

      runForAsset(src2) shouldBe 'left

      runScript[EVALUATED](src1, Coproduct[In](in)) shouldBe Right(CONST_BOOLEAN(true))
      runScript[EVALUATED](src2, Coproduct[In](in)) shouldBe Right(CONST_LONG(1))
    }
  }

  def runForAsset(script: String): Either[String, EVALUATED] = {
    import cats.syntax.monoid._
    import com.wavesplatform.lang.v1.CTX._

    val Success(expr, _) = Parser.parseExpr(script)
    val ctx =
      PureContext.build(Global, V2).withEnvironment[Environment] |+|
      CryptoContext.build(Global, V2).withEnvironment[Environment] |+|
      WavesContext.build(DirectiveSet(V2, Asset, Expression).explicitGet())

    val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, Coeval(null))
    for {
      compileResult <- compiler.ExpressionCompiler(ctx.compilerContext, expr)
      (typedExpr, _) = compileResult
      r <- EvaluatorV1().apply[EVALUATED](ctx.evaluationContext(environment), typedExpr)
    } yield r
  }

  def runWithSmartTradingActivated(script: String, t: In = null, chainId: Byte = chainId): Either[String, EVALUATED] = {
    import cats.syntax.monoid._
    import com.wavesplatform.lang.v1.CTX._

    val Success(expr, _) = Parser.parseExpr(script)
    val environment = new WavesEnvironment(chainId, Coeval(t), null, EmptyBlockchain, Coeval(null))

    val ctx =
      PureContext.build(Global, V2).withEnvironment[Environment] |+|
      CryptoContext.build(Global, V2).withEnvironment[Environment] |+|
      WavesContext.build(
        DirectiveSet(V2, Account, Expression).explicitGet(),
      )

    for {
      compileResult <- ExpressionCompiler(ctx.compilerContext, expr)
      (typedExpr, _) = compileResult
      r <- EvaluatorV1().apply[EVALUATED](ctx.evaluationContext(environment), typedExpr)
    } yield r
  }
}
