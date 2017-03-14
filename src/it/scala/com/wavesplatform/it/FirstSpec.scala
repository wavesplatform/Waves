package com.wavesplatform.it

import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.ContainerConfig
import org.scalatest._

class FirstSpec(docker: DockerClient) extends FreeSpec with BeforeAndAfterAllConfigMap {
  "zzz" - {
    "works" in {
      val cfg = ContainerConfig.builder().image(System.getProperty("docker.imageId")).build()
      val containerId = docker.createContainer(cfg).id()

      docker.startContainer(containerId)
    }
  }
}
