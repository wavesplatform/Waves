package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatureStatus.{Activated, Approved, Undefined}
import com.wavesplatform.features.api.NodeFeatureStatus.{Implemented, NotImplemented, Voted}
import play.api.libs.json._

package object api {
  implicit val nodeFeatureStatusFormat: Writes[NodeFeatureStatus] =
    new Writes[NodeFeatureStatus] {
      private val notimplemented = "NOT_IMPLEMENTED"
      private val implemented    = "IMPLEMENTED"
      private val voted          = "VOTED"

      override def writes(o: NodeFeatureStatus): JsValue = {
        o match {
          case NotImplemented => JsString(notimplemented)
          case Implemented    => JsString(implemented)
          case Voted          => JsString(voted)
        }
      }
    }

  implicit val blockchainFeatureStatusFormat: Writes[BlockchainFeatureStatus] =
    new Writes[BlockchainFeatureStatus] {
      private val undefined = "VOTING"
      private val approved  = "APPROVED"
      private val activated = "ACTIVATED"

      override def writes(o: BlockchainFeatureStatus): JsValue = {
        o match {
          case Undefined => JsString(undefined)
          case Approved  => JsString(approved)
          case Activated => JsString(activated)
        }
      }
    }

  implicit val activationStatusFeatureFormat: Writes[FeatureActivationStatus] = Json.writes
  implicit val activationStatusFormat: Writes[ActivationStatus]               = Json.writes
}
