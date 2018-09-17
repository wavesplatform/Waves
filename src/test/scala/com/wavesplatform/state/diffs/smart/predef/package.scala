package com.wavesplatform.state.diffs.smart

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.smart.BlockchainContext
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, Transaction}
import com.wavesplatform.utils.dummyCompilerContext
import fastparse.core.Parsed.Success
import monix.eval.Coeval

package object predef {
  val networkByte: Byte = 'u'

  def runScript[T](script: String, tx: Transaction = null, networkByte: Byte = networkByte): Either[String, T] = {
    val Success(expr, _) = Parser(script)
    for {
      compileResult <- CompilerV1(dummyCompilerContext, expr)
      (typedExpr, tpe) = compileResult
      er               = EvaluatorV1[T](BlockchainContext.build(networkByte, Coeval(tx), Coeval(???), null), typedExpr)
      r <- er._2
    } yield r
  }

  def scriptWithAllFunctions(tx: DataTransaction, t: TransferTransaction): String =
    s"""
       | # Pure context
       | # 1) basic(+ eq) -> mulLong, divLong, modLong, sumLong, subLong, sumString, sumByteVector
       |
       | let rnd = tx.timestamp % 2 == 0
       | let longAll = 1000 * 2 == 2000 && 1000 / 2 == 500 && 1000 % 2 == 0 && 1000 + 2 == 1002 && 1000 - 2 == 998
       | let sumString = "ha" + "-" +"ha" == "ha-ha"
       | let sumByteVector = match tx {
       |     case d: DataTransaction =>
       |      let body = d.bodyBytes
       |      body + base64'${ByteStr(tx.bodyBytes.apply()).base64}' == base64'${ByteStr(tx.bodyBytes.apply()).base64}' + base64'${ByteStr(
         tx.bodyBytes.apply()).base64}'
       |     case d: TransferTransaction => true
       |     case _ => false
       |   }
       |
       | let eqUnion = match tx {
       |   case d: DataTransaction => true
       |   case d: TransferTransaction => d.recipient == Address(base58'${t.recipient.bytes.base58}')
       |   case _ => false
       | }
       |   
       | let basic = longAll && sumString && sumByteVector && eqUnion
       | 
       | # 2) ne
       | let nePrim = 1000 != 999 && "ha" +"ha" != "ha-ha" && tx.bodyBytes != base64'hahaha'
       | let neDataEntryAndGetElement = match tx {
       |    case d: DataTransaction => d.data[0] != DataEntry("ha", true)
       |    case d: TransferTransaction => true
       |    case _ => false
       |  }
       |
       | let neOptionAndExtractHeight = match tx {
       |   case d: DataTransaction => true
       |   case d: TransferTransaction => extract(transactionHeightById(tx.id)) != 0
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
       |    case d: DataTransaction => size(d.data) != 0
       |    case d: TransferTransaction => true
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
       |     case d: DataTransaction =>
       |       size(d.bodyBytes) != 0 && take(d.bodyBytes, 1) != base58'ha' && drop(d.bodyBytes, 1) != base58'ha' && takeRight(d.bodyBytes, 1) != base58'ha' && dropRight(d.bodyBytes, 1) != base58'ha'
       |     case d: TransferTransaction => isDefined(d.feeAssetId) == false
       |     case _ => false
       |   }
       | let strOps = size("haha") != 0 && take("haha", 1) != "" && drop("haha", 0) != "" && takeRight("haha", 1) != "" && dropRight("haha", 0) != ""
       |
       | let pure = basic && ne && gteLong && getListSize && unary && frAction && bytesOps && strOps
       |
       | # Waves context
       | let txById = match tx {
       |     case d: DataTransaction => true
       |     case d: TransferTransaction =>
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
       |   case t: TransferTransaction =>
       |     let add = Address(base58'${t.recipient.bytes.base58}')
       |     let long = extract(getInteger(add,"${tx.data(0).key}")) == ${tx.data(0).value}
       |     let bool = extract(getBoolean(add,"${tx.data(1).key}")) == ${tx.data(1).value}
       |     let bin = extract(getBinary(add,"${tx.data(2).key}")) ==  base58'${tx.data(2).value}'
       |     let str = extract(getString(add,"${tx.data(3).key}")) == "${tx.data(3).value}"
       |     long && bool && bin && str
       |
       |   case a: CreateAliasTransaction => throw("oh no")
       |   case b: BurnTransaction => throw()
       |   case _ => false
       | }
       |
       | let aFromPK = addressFromPublicKey(tx.senderPublicKey) == tx.sender
       | let aFromStrOrRecip = match tx {
       |   case d: DataTransaction => addressFromString("${tx.sender.address}") == Address(base58'${tx.sender.bytes.base58}')
       |   case d: TransferTransaction => addressFromRecipient(d.recipient) == Address(base58'${t.recipient.bytes.base58}')
       |   case _ => false
       | }
       |
       | let balances = assetBalance(tx.sender, unit) > 0 && wavesBalance(tx.sender) != 0
       | let block = lastBlock.height > 0 && lastBlock.timestamp > 0 && size(lastBlock.generationSignature) > 0
       |
       | let waves = txById && entries && balances && aFromPK && aFromStrOrRecip && block
       |
       | # Crypto context
       | let bks = blake2b256(base58'') != base58'' && keccak256(base58'') != base58'' && sha256(base58'') != base58''
       | let sig = sigVerify(base58'333', base58'123', base58'567') != true
       | let str58 = fromBase58String(toBase58String(tx.id)) == tx.id
       | let str64 = fromBase64String(toBase64String(tx.id)) == tx.id
       |
       | let crypto = bks && sig && str58 && str64
       |
       | if rnd then pure && waves else crypto
    """.stripMargin

}
