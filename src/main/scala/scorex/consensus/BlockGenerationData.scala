package scorex.consensus

import play.api.libs.json.JsObject
import scorex.block.Block

trait BlockGenerationData {
  def toBytes: Array[Byte]
  def toJson:JsObject
  def isGenesis:Boolean
  def signature:Array[Byte]
  def isValid(block: Block): Boolean
  def isSignatureValid(block:Block):Boolean
}