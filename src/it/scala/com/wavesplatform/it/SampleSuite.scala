package com.wavesplatform.it

import org.scalatest.{FreeSpec, Suite}

import scala.collection.immutable.IndexedSeq

class SampleSuite extends FreeSpec with DockerSupport {
  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(new FirstSpec(docker))
}
