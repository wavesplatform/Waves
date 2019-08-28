package com.wavesplatform.lang.v1.repl

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.TreeNode
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.node.NullNode
import com.fasterxml.jackson.databind.node.ObjectNode
import java.io.IOException

import com.wavesplatform.lang.v1.repl.model.transactions.Transaction

case class TransactionDeserializer(objectMapper: ObjectMapper) extends StdDeserializer[Transaction]() {
  @throws[IOException]
  override def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext): Nothing = {
    val treeNode = jsonParser.getCodec.readTree(jsonParser)
    val `type`   = objectMapper.treeToValue(treeNode.get("type"), classOf[Integer])
    val version  = objectMapper.treeToValue(treeNode.get("version"), classOf[Integer])
    var chainId  = objectMapper.getChainId
    if (treeNode.get("chainId") != null && !treeNode.get("chainId").isInstanceOf[NullNode]) {
      val _chainId = objectMapper.treeToValue(treeNode.get("chainId"), classOf[Byte])
      if (_chainId != null) chainId = _chainId
    }
// todo omfg remove after 0.15.4 release
    treeNode.asInstanceOf[ObjectNode].put("chainId", chainId)
    var t = null
    `type` match {
      case AliasTransaction.ALIAS =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case BurnTransaction.BURN =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case DataTransaction.DATA =>
        t = classOf[Nothing]
        break //todo: break is not supported
      case IssueTransaction.ISSUE =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case LeaseCancelTransaction.LEASE_CANCEL =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case LeaseTransaction.LEASE =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case MassTransferTransaction.MASS_TRANSFER =>
        t = classOf[Nothing]
        break //todo: break is not supported
      case ReissueTransaction.REISSUE =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case SetScriptTransaction.SET_SCRIPT =>
        t = classOf[Nothing]
        break //todo: break is not supported
      case SponsorTransaction.SPONSOR =>
        t = classOf[Nothing]
        break //todo: break is not supported
      case ExchangeTransactionV1.EXCHANGE =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case TransferTransaction.TRANSFER =>
        version match {
          case Transaction.V1 =>
            t = classOf[Nothing]
            break //todo: break is not supported
          case Transaction.V2 =>
            t = classOf[Nothing]
            break //todo: break is not supported
        }
        break //todo: break is not supported
      case InvokeScriptTransaction.CONTRACT_INVOKE =>
        t = classOf[Nothing]
        break //todo: break is not supported
      case SetAssetScriptTransaction.SET_ASSET_SCRIPT =>
        t = classOf[Nothing]
        break //todo: break is not supported
      case _ =>
        t = classOf[Nothing]
    }
    objectMapper.treeToValue(treeNode, t).asInstanceOf[Nothing]
  }
}
