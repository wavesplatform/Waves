package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatureStatus.{Activated, Approved, Undefined}
import com.wavesplatform.features.api.NodeFeatureStatus.{Implemented, NotImplemented, Voted}
import play.api.libs.json._

package object api {
  implicit val nodeFeatureStatusFormat: Format[NodeFeatureStatus] =
    new Format[NodeFeatureStatus] {
      private val notimplemented = "NOT_IMPLEMENTED"
      private val implemented    = "IMPLEMENTED"
      private val voted          = "VOTED"

      override def reads(json: JsValue): JsResult[NodeFeatureStatus] =
        json match {
          case JsString(`notimplemented`) => JsSuccess(NotImplemented)
          case JsString(`implemented`)    => JsSuccess(Implemented)
          case JsString(`voted`)          => JsSuccess(Voted)
          case _                          => ???
        }

      override def writes(o: NodeFeatureStatus): JsValue = {
        o match {
          case NotImplemented => JsString(notimplemented)
          case Implemented    => JsString(implemented)
          case Voted          => JsString(voted)
        }
      }
    }

  implicit val blockchainFeatureStatusFormat: Format[BlockchainFeatureStatus] =
    new Format[BlockchainFeatureStatus] {
      private val undefined = "VOTING"
      private val approved  = "APPROVED"
      private val activated = "ACTIVATED"

      override def reads(json: JsValue): JsResult[BlockchainFeatureStatus] =
        json match {
          case JsString(`undefined`) => JsSuccess(Undefined)
          case JsString(`approved`)  => JsSuccess(Approved)
          case JsString(`activated`) => JsSuccess(Activated)
          case _                     => ???
        }

      override def writes(o: BlockchainFeatureStatus): JsValue = {
        o match {
          case Undefined => JsString(undefined)
          case Approved  => JsString(approved)
          case Activated => JsString(activated)
        }
      }
    }

  implicit val activationStatusFeatureFormat: Format[FeatureActivationStatus] = Json.format
  implicit val activationStatusFormat: Format[ActivationStatus]               = Json.format
}
