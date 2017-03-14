package com.wavesplatform.it

import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import org.scalatest.{BeforeAndAfterAll, Suite}

trait DockerSupport extends BeforeAndAfterAll { this: Suite =>
  val imageId: String = System.getProperty("docker.imageId")
  val docker: DockerClient = DefaultDockerClient.fromEnv().build()

  override protected def afterAll(): Unit = docker.close()
}
