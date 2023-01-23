package com.wavesplatform.lang.impl

import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.{Object, Promise, native}

@native
@JSGlobalScope
object Global extends Object {
  def curve25519verify(message: ArrayBuffer, sig: ArrayBuffer, pub: ArrayBuffer): Boolean                = native
  def rsaVerify(alg: DigestAlgorithm, message: ArrayBuffer, sig: ArrayBuffer, pub: ArrayBuffer): Boolean = native
  def keccak256(message: ArrayBuffer): ArrayBuffer                                                       = native
  def blake2b256(message: ArrayBuffer): ArrayBuffer                                                      = native
  def sha256(message: ArrayBuffer): ArrayBuffer                                                          = native
  def merkleVerify(root: ArrayBuffer, proof: ArrayBuffer, data: ArrayBuffer): Boolean                    = native

  def httpGet(params: js.Dynamic): Promise[js.Dynamic] = native
}
