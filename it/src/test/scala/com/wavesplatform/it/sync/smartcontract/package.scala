package com.wavesplatform.it.sync
import com.wavesplatform.transaction.DataTransaction

package object smartcontract {
  def cryptoContext(dtx: DataTransaction) =
    Some(s"""
       |match tx {
       |  case ext : ExchangeTransaction =>
       |    # Crypto context
       |    let bks = blake2b256(base58'') != base58'' && keccak256(base58'') != base58'' && sha256(base58'') != base58''
       |    let sig = sigVerify(base58'333', base58'123', base58'567') != true
       |    let str58 = fromBase58String(toBase58String(tx.id)) == tx.id
       |    let str64 = fromBase64String(toBase64String(tx.id)) == tx.id
       |    bks && sig && str58 && str64
       |  case s : SetScriptTransaction => true
       |  case _ => false
       |}
     """.stripMargin)

  def checkExtract(dtx: DataTransaction) =
    Some(s"""
       |match tx {
       |  case ext : ExchangeTransaction =>
       |     extract(transactionHeightById(base58'${dtx.id().base58}')) > 0
       |  case _ => false
       |}
     """.stripMargin)

  def pureContext(dtx: DataTransaction) =
    Some(s"""
       | match tx {
       |  case ext : ExchangeTransaction =>
       |    # Pure context
       |    let longAll = 1000 * 2 == 2000 && 1000 / 2 == 500 && 1000 % 2 == 0 && 1000 + 2 == 1002 && 1000 - 2 == 998
       |    let sumString = "ha" + "-" +"ha" == "ha-ha"
       |    let body = ext.bodyBytes
       |    let sumByteVector = body + ext.bodyBytes == body + body
       |
       |    let eqUnion = ext.sender != Address(base58'')
       |    let basic = longAll && sumString && sumByteVector && eqUnion
       |
       |    let nePrim = 1000 != 999 && "ha" +"ha" != "ha-ha" && ext.bodyBytes != base64'hahaha'
       |    let dtx = extract(transactionById(base58'${dtx.id().base58}'))
       |    let neDataEntryAndGetElement = match dtx {
       |       case ddtx : DataTransaction => ddtx.data[0] != DataEntry("ha", true)
       |       case _ => false
       |    }
       |    let neOptionAndExtractHeight = dtx.id != base58'' # shouldbe extract(transactionHeightById(base58'${dtx.id().base58}')) > 0
       |
       |    let ne = nePrim && neDataEntryAndGetElement && neOptionAndExtractHeight
       |    let gteLong = 1000 > 999 && 1000 >= 999
       |
       |    let getListSize = match dtx {
       |      case dddtx : DataTransaction => size(dddtx.data) != 0
       |      case _ => false
       |    }
       |
       |    let unary = -1 == -1 && false == !true
       |
       |    let frAction = fraction(12, 3, 4) == 9
       |    let bytesOps = size(ext.bodyBytes) != 0 && take(ext.bodyBytes, 1) != base58'ha' && drop(ext.bodyBytes, 1) != base58'ha' &&
       |    takeRight(ext.bodyBytes, 1) != base58'ha' && dropRight(ext.bodyBytes, 1) != base58'ha'
       |    let strOps = size("haha") != 0 && take("haha", 1) != "" && drop("haha", 0) != "" && takeRight("haha", 1) != "" &&
       |    dropRight("haha", 0) != ""
       |
       |    let pure = basic && ne && gteLong && getListSize && unary && frAction #&& bytesOps && strOps
       |
       |    pure && height > 0
       |  case s : SetScriptTransaction => true
       |  case _ => false
       | }
     """.stripMargin)

  def wavesContext(dtx: DataTransaction) =
    Some(s"""
       | match tx {
       |  case ext : ExchangeTransaction =>
       |    # Waves context
       |    let dtx = extract(transactionById(base58'${dtx.id().base58}'))
       |    let entries = match dtx {
       |       case d: DataTransaction =>
       |         let int = extract(getInteger(d.data, "${dtx.data(0).key}"))
       |         let bool = extract(getBoolean(d.data, "${dtx.data(1).key}"))
       |         let blob = extract(getBinary(d.data, "${dtx.data(2).key}"))
       |         let str = extract(getString(d.data, "${dtx.data(3).key}"))
       |         let dataByKey = toString(int) == "${dtx.data(0).value}" || toString(bool) == "${dtx.data(1).value}" ||
       |                     size(blob) > 0 || str == "${dtx.data(3).value}"
       |
       |         let d0 = extract(getInteger(d.data, 0))
       |         let d1 = extract(getBoolean(d.data, 1))
       |         let d2 = getBinary(d.data, 2)
       |         let d3 = getString(d.data, 3)
       |         let dataByIndex = toBytes(d0) == base64'abcdef' || toBytes(d1) == base64'ghijkl' ||
       |                       isDefined(d2) || toBytes(extract(d3)) == base64'mnopqr'
       |
       |         let add = Address(base58'${dtx.sender.bytes.base58}')
       |         let long = extract(getInteger(add,"${dtx.data(0).key}")) == ${dtx.data(0).value}
       |         let bool1 = extract(getBoolean(add,"${dtx.data(1).key}")) == ${dtx.data(1).value}
       |         let bin = extract(getBinary(add,"${dtx.data(2).key}")) ==  base58'${dtx.data(2).value}'
       |         let str1 = extract(getString(add,"${dtx.data(3).key}")) == "${dtx.data(3).value}"
       |
       |         long && bool1 && bin && str1 && dataByKey && dataByIndex
       |     case _ => false
       |     }
       |
       |     let aFromPK = addressFromPublicKey(ext.senderPublicKey) == ext.sender
       |     let aFromStr = addressFromString("${dtx.sender.address}") == Address(base58'${dtx.sender.bytes.base58}')
       |
       |     #case t1: TransferTransaction => addressFromRecipient(t1.recipient) == Address(base58'')
       |
       |     let balances = assetBalance(ext.sender, unit) > 0 && wavesBalance(ext.sender) != 0
       |
       |     entries && balances && aFromPK && aFromStr && height > 0
       |  case s : SetScriptTransaction => true
       |  case _ => false
       | }
     """.stripMargin)
}
