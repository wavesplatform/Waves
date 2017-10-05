package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatureStatus.{Accepted, Activated, Undefined}
import com.wavesplatform.features.api.NodeFeatureStatus.{Supported, Unsupported}
import play.api.libs.json._

package object api {
  implicit val nodeFeatureStatusFormat: Format[NodeFeatureStatus] =
    new Format[NodeFeatureStatus] {
      private val unsupported = "UNSUPPORTED"
      private val supported = "SUPPORTED"

      override def reads(json: JsValue): JsResult[NodeFeatureStatus] =
        json match {
          case JsString(`unsupported`) => JsSuccess(Unsupported)
          case JsString(`supported`) => JsSuccess(Supported)
          case _ => ???
        }

      override def writes(o: NodeFeatureStatus): JsValue = {
        o match {
          case Unsupported => JsString(unsupported)
          case Supported => JsString(supported)
        }
      }
    }

  implicit val blockchainFeatureStatusFormat: Format[BlockchainFeatureStatus] =
    new Format[BlockchainFeatureStatus] {
      private val undefined = "VOTING"
      private val accepted = "ACCEPTED"
      private val activated = "ACTIVATED"

      override def reads(json: JsValue): JsResult[BlockchainFeatureStatus] =
        json match {
          case JsString(`undefined`) => JsSuccess(Undefined)
          case JsString(`accepted`) => JsSuccess(Accepted)
          case JsString(`activated`) => JsSuccess(Activated)
          case _ => ???
        }

      override def writes(o: BlockchainFeatureStatus): JsValue = {
        o match {
          case Undefined => JsString(undefined)
          case Accepted => JsString(accepted)
          case Activated => JsString(activated)
        }
      }
    }

  implicit val activationStatusFeatureFormat: Format[ActivationStatusFeature] = Json.format
  implicit val activationStatusFormat: Format[ActivationStatus] = Json.format
}
