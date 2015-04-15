package scorex.consensus

import play.api.libs.json.JsObject
import scorex.block.Block

trait BlockGenerationData {
  def toBytes: Array[Byte]

  def toJson: JsObject

  def isGenesis: Boolean

  def signature: Array[Byte]

  def isValid(block: Block): Boolean

  def isSignatureValid(block: Block): Boolean

  //block score value, where blockchain quality measure is the sum of block scores.
  //So block score is e.g. 1 in case of longest chain rule, 2^64/baseTarget in case of Nxt's cumulative difficulty etc
  def blockScore():BigInt
}