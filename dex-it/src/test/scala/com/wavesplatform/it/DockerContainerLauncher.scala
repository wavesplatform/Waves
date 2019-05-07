package com.wavesplatform.it

import cats.implicits._
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, PortBinding}
import com.wavesplatform.it.DockerContainerLauncher.{ContainerIsNotStartedYetError, DockerError}

import scala.collection.JavaConverters._
import scala.util.Try

class DockerContainerLauncher(imageName: String,
                              containerName: String,
                              env: String,
                              containerPort: String,
                              hostPort: Option[String] = None,
                              imageTag: String = "latest") {

  val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  private val hostConfig = {
    val hostConfigBuilder = HostConfig.builder()
    hostPort
      .fold(hostConfigBuilder.publishAllPorts(true)) { port =>
        hostConfigBuilder.portBindings(Map(containerPort -> List(PortBinding.of("0.0.0.0", port)).asJava).asJava)
      }
      .build()
  }

  private val containerConfig = {
    dockerClient.pull(s"$imageName:$imageTag")
    ContainerConfig
      .builder()
      .hostConfig(hostConfig)
      .exposedPorts(containerPort)
      .image(imageName)
      .env(env)
      .build()
  }

  private val creation    = dockerClient.createContainer(containerConfig, containerName)
  val containerId: String = creation.id()

  def getHostPort: Either[DockerError, String] = {
    hostPort.fold {
      Either
        .fromTry(Try { dockerClient.inspectContainer(containerId).networkSettings().ports().asScala(s"$containerPort/tcp").asScala.head.hostPort() })
        .leftMap[DockerError](ex => ContainerIsNotStartedYetError(s"Container with name $containerName is not started yet, ex: ${ex.getMessage}"))
    } { _.asRight[DockerError] }
  }

  def startContainer(): Unit = dockerClient.startContainer(containerId)

  def stopAndRemoveContainer(): Unit = {
    dockerClient.stopContainer(containerId, 0)
    dockerClient.removeContainer(containerId, RemoveContainerParam.removeVolumes())
  }
}

object DockerContainerLauncher {
  trait DockerError                                   extends Product with Serializable
  case class ContainerIsNotStartedYetError(m: String) extends DockerError
}
