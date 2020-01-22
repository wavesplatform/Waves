package com.wavesplatform.state.diffs.smart

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.smart.BlockchainContext
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, Transaction}
import com.wavesplatform.utils.EmptyBlockchain
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import shapeless.Coproduct

package object predef {
  val chainId: Byte = 'u'

  def runScript[T <: EVALUATED](script: String, version: StdLibVersion, t: In, blockchain: Blockchain, chainId: Byte): Either[String, T] = {
    val Success(expr, _) = Parser.parseExpr(script)
    for {
      compileResult <- ExpressionCompiler(compilerContext(version, Expression, isAssetScript = false), expr)
      (typedExpr, _) = compileResult
      evalContext <- BlockchainContext.build(version,
                                             chainId,
                                             Coeval.evalOnce(t),
                                             Coeval.evalOnce(blockchain.height),
                                             blockchain,
                                             isTokenContext = false,
                                             isContract = false,
                                             Coeval(???))
      r <- EvaluatorV1().apply[T](evalContext, typedExpr)
    } yield r
  }

  def runScript[T <: EVALUATED](script: String, t: In = null, ctxV: StdLibVersion = V1): Either[String, T] =
    runScript[T](script, ctxV, t, EmptyBlockchain, chainId)

  def runScript[T <: EVALUATED](script: String, t: In, chainId: Byte): Either[String, T] =
    runScript[T](script, V1, t, EmptyBlockchain, chainId)

  def runScript[T <: EVALUATED](script: String, tx: Transaction, blockchain: Blockchain): Either[String, T] =
    runScript[T](script, V1, Coproduct(tx), blockchain, chainId)

  def runScriptWithCustomContext[T <: EVALUATED](script: String, t: In, chainId: Byte, ctxV: StdLibVersion = V1): Either[String, T] =
    runScript[T](script, ctxV, t, EmptyBlockchain, chainId)

  private def dropLastLine(str: String): String = str.replace("\r", "").split('\n').init.mkString("\n")

  def scriptWithAllV1Functions(tx: DataTransaction, t: TransferTransaction): String =
    s"""${dropLastLine(scriptWithV1PureFunctions(tx, t))}
       |${dropLastLine(scriptWithV1WavesFunctions(tx, t))}
       |${dropLastLine(scriptWithCryptoFunctions)}
       |if rnd then pure && waves else crypto""".stripMargin

  def scriptWithV1PureFunctions(tx: DataTransaction, t: TransferTransaction): String =
    s"""
       | # Pure context
       | # 1) basic(+ eq) -> mulLong, divLong, modLong, sumLong, subLong, sumString, sumByteVector
       |
       | let rnd = tx.timestamp % 2 == 0
       | let longAll = 1000 * 2 == 2000 && 1000 / 2 == 500 && 1000 % 2 == 0 && 1000 + 2 == 1002 && 1000 - 2 == 998
       | let sumString = "ha" + "-" +"ha" == "ha-ha"
       | let sumByteVector = match tx {
       |     case d0: DataTransaction =>
       |      let body = d0.bodyBytes
       |      body + base64'${ByteStr(tx.bodyBytes.apply()).base64}' == base64'${ByteStr(tx.bodyBytes.apply()).base64}' + base64'${ByteStr(
         tx.bodyBytes.apply()).base64}'
       |     case _: TransferTransaction => true
       |     case _ => false
       |   }
       |
       | let eqUnion = match tx {
       |   case _: DataTransaction => true
       |   case t0: TransferTransaction => t0.recipient == Address(base58'${t.recipient.bytes.base58}')
       |   case _ => false
       | }
       |
       | let basic = longAll && sumString && sumByteVector && eqUnion
       |
       | # 2) ne
       | let nePrim = 1000 != 999 && "ha" +"ha" != "ha-ha" && tx.bodyBytes != base64'hahaha'
       | let neDataEntryAndGetElement = match tx {
       |    case d1: DataTransaction => d1.data[0] != DataEntry("ha", true)
       |    case _: TransferTransaction => true
       |    case _ => false
       |  }
       |
       | let neOptionAndExtractHeight = match tx {
       |   case _: DataTransaction => true
       |   case _: TransferTransaction => extract(transactionHeightById(tx.id)) != 0
       |   case _ => false
       | }
       |
       | let ne = nePrim && neDataEntryAndGetElement && neOptionAndExtractHeight
       |
       |# 3) gt, ge
       | let gteLong = 1000 > 999 && 1000 >= 999
       |
       |# 4) getListSize
       | let getListSize = match tx {
       |    case d2: DataTransaction => size(d2.data) != 0
       |    case _: TransferTransaction => true
       |    case _ => false
       |  }
       |
       |# 5) unary
       | let unary = -1 == -1 && false == !true
       |
       |# 6) fraction, sizeBytes, takeBytes, dropBytes, takeRightBytes, dropRightBytes, sizeString, takeString, dropString,
       |#    takeRightString, dropRightString, isDefined
       | let frAction = fraction(12, 3, 4) == 9
       | let bytesOps = match tx {
       |     case d3: DataTransaction =>
       |       size(d3.bodyBytes) != 0 && take(d3.bodyBytes, 1) != base58'ha' && drop(d3.bodyBytes, 1) != base58'ha' && takeRight(d3.bodyBytes, 1) != base58'ha' && dropRight(d3.bodyBytes, 1) != base58'ha'
       |     case t1: TransferTransaction => isDefined(t1.feeAssetId) == false
       |     case _ => false
       |   }
       | let strOps = size("haha") != 0 && take("haha", 1) != "" && drop("haha", 0) != "" && takeRight("haha", 1) != "" && dropRight("haha", 0) != ""
       |
       | let pure = basic && ne && gteLong && getListSize && unary && frAction && bytesOps && strOps
       | pure""".stripMargin

  def scriptWithV1WavesFunctions(tx: DataTransaction, t: TransferTransaction): String =
    s""" # Waves context
       | let txById = match tx {
       |     case _: DataTransaction => true
       |     case _: TransferTransaction =>
       |       let g = extract(transactionById(base58'${tx.id().base58}'))
       |       g.id == base58'${tx.id().base58}'
       |     case _ => false
       | }
       | let entries = match tx {
       |   case d: DataTransaction =>
       |     let int = extract(getInteger(d.data, "${tx.data(0).key}"))
       |     let bool = extract(getBoolean(d.data, "${tx.data(1).key}"))
       |     let blob = extract(getBinary(d.data, "${tx.data(2).key}"))
       |     let str = extract(getString(d.data, "${tx.data(3).key}"))
       |     let dataByKey = toString(int) == "${tx.data(0).value}" || toString(bool) == "${tx.data(1).value}" ||
       |                     size(blob) > 0 || str == "${tx.data(3).value}"
       |
       |     let d0 = extract(getInteger(d.data, 0))
       |     let d1 = extract(getBoolean(d.data, 1))
       |     let d2 = getBinary(d.data, 2)
       |     let d3 = getString(d.data, 3)
       |     let dataByIndex = toBytes(d0) == base64'abcdef' || toBytes(d1) == base64'ghijkl' ||
       |                       isDefined(d2) || toBytes(extract(d3)) == base64'mnopqr'
       |
       |     dataByKey && dataByIndex
       |
       |   case _: TransferTransaction =>
       |     let add = Address(base58'${t.recipient.bytes.base58}')
       |     let long = extract(getInteger(add,"${tx.data(0).key}")) == ${tx.data(0).value}
       |     let bool1 = extract(getBoolean(add,"${tx.data(1).key}")) == ${tx.data(1).value}
       |     let bin = extract(getBinary(add,"${tx.data(2).key}")) ==  base58'${tx.data(2).value}'
       |     let str1 = extract(getString(add,"${tx.data(3).key}")) == "${tx.data(3).value}"
       |     long && bool1 && bin && str1
       |
       |   case a: CreateAliasTransaction => throw("oh no")
       |   case b: BurnTransaction => throw()
       |   case _ => false
       | }
       |
       | let aFromPK = addressFromPublicKey(tx.senderPublicKey) == tx.sender
       | let aFromStrOrRecip = match tx {
       |   case _: DataTransaction => addressFromString("${tx.sender.stringRepr}") == Address(base58'${tx.sender.bytes.base58}')
       |   case t1: TransferTransaction => addressFromRecipient(t1.recipient) == Address(base58'${t.recipient.bytes.base58}')
       |   case _ => false
       | }
       |
       | let balances = assetBalance(tx.sender, unit) > 0 && wavesBalance(tx.sender) != 0
       |
       | let waves = txById && entries && balances && aFromPK && aFromStrOrRecip && height > 0
       | waves""".stripMargin

  def scriptWithCryptoFunctions: String =
    s"""
       | # Crypto context
       | let bks = blake2b256(base58'') != base58'' && keccak256(base58'') != base58'' && sha256(base58'') != base58''
       | let sig = sigVerify(base58'333', base58'123', base58'567') != true
       | let str58 = fromBase58String(toBase58String(tx.id)) == tx.id
       | let str64 = fromBase64String(toBase64String(tx.id)) == tx.id
       |
       | let crypto = bks && sig && str58 && str64
       | crypto""".stripMargin

}
