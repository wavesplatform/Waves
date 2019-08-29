package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.{JsonParser, TreeNode}
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.node.{NullNode, ObjectNode}
import com.fasterxml.jackson.databind.{DeserializationContext, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.wavesplatform.lang.v1.repl.model.Transaction
import com.wavesplatform.lang.v1.repl.model.transaction._

case class TransactionDeserializer(
  objectMapper: ObjectMapper with ScalaObjectMapper,
  chainIdP: Byte
) extends StdDeserializer(classOf[Option[Transaction]]) {
  val notExistCode = 311

  override def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext): Option[Transaction] = {
    val treeNode: TreeNode = jsonParser.getCodec.readTree(jsonParser)
    val errorField = treeNode.get("error")
    if (errorField == null)
      Some(deserializeTx(treeNode))
    else {
      val code = objectMapper.treeToValue[Int](errorField)
      if (code == notExistCode) None
      else {
        val message = objectMapper.treeToValue[String](treeNode.get("message"))
        throw new RuntimeException(s"Error when deserializing tx: code=$code, $message")
      }
    }
  }

  private def deserializeTx(treeNode: TreeNode): Transaction = {
    val `type`   = objectMapper.treeToValue[Int](treeNode.get("type"))
    val version  = objectMapper.treeToValue[Int](treeNode.get("version"))

    val chainId =
      if (!treeNode.get("chainId").isInstanceOf[NullNode] && treeNode.get("chainId") != null) objectMapper.treeToValue[Byte](treeNode.get("chainId"))
      else chainIdP

    treeNode.asInstanceOf[ObjectNode].put("chainId", chainId)
    val t = `type` match {
      case AliasTransaction.ALIAS =>
        version match {
          case Transaction.V1 => classOf[AliasTransactionV1]
          case Transaction.V2 => classOf[AliasTransactionV2]
        }
      case BurnTransaction.BURN =>
        version match {
          case Transaction.V1 => classOf[BurnTransactionV1]
          case Transaction.V2 => classOf[BurnTransactionV2]
        }
      case DataTransaction.DATA => classOf[DataTransaction]
      case IssueTransaction.ISSUE =>
        version match {
          case Transaction.V1 => classOf[IssueTransactionV1]
          case Transaction.V2 => classOf[IssueTransactionV2]
        }
      case LeaseCancelTransaction.LEASE_CANCEL =>
        version match {
          case Transaction.V1 => classOf[LeaseCancelTransactionV1]
          case Transaction.V2 => classOf[LeaseCancelTransactionV2]
        }
      case LeaseTransaction.LEASE =>
        version match {
          case Transaction.V1 => classOf[LeaseTransactionV1]
          case Transaction.V2 => classOf[LeaseTransactionV2]
        }
      case MassTransferTransaction.MASS_TRANSFER => classOf[MassTransferTransaction]
      case ReissueTransaction.REISSUE =>
        version match {
          case Transaction.V1 => classOf[ReissueTransactionV1]
          case Transaction.V2 => classOf[ReissueTransactionV2]
        }
      case SetScriptTransaction.SET_SCRIPT => classOf[SetScriptTransaction]
      case SponsorTransaction.SPONSOR => classOf[SponsorTransaction]
      case ExchangeTransaction.EXCHANGE =>
        version match {
          case Transaction.V1 => classOf[ExchangeTransactionV1]
          case Transaction.V2 => classOf[ExchangeTransactionV2]
        }
      case TransferTransaction.TRANSFER =>
        version match {
          case Transaction.V1 => classOf[TransferTransactionV1]
          case Transaction.V2 => classOf[TransferTransactionV2]
        }
      case InvokeScriptTransaction.CONTRACT_INVOKE => classOf[InvokeScriptTransaction]
      case SetAssetScriptTransaction.SET_ASSET_SCRIPT => classOf[SetAssetScriptTransaction]
      case _ => classOf[UnknownTransaction]
    }
    objectMapper.treeToValue(treeNode, t).asInstanceOf[Transaction]
  }
}
