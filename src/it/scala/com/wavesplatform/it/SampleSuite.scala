package com.wavesplatform.it

import org.scalatest.{BeforeAndAfterAll, FreeSpec, Suite}

import scala.collection.immutable.IndexedSeq

class SampleSuite extends FreeSpec with BeforeAndAfterAll {
  private val docker = Docker()

  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(new ValidChainGenerationSpec(docker))

  override protected def afterAll() = docker.close()
}
