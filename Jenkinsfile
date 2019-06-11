pipeline {
    agent {
        label 'buildagent'
    }
    environment {
        // Define sbt path
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = 7
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
    }
    options {
        ansiColor('xterm')
    }
    stages {
        stage('Unit Test') {
            steps {
                sh "sbt -mem 10240 checkPR"
            }
        }
        stage('Check containers') {
            steps {
                sh 'docker rmi com.wavesplatform/it com.wavesplatform/node-it com.wavesplatform/dex-it || true'
                sh 'docker ps -a'
                sh 'docker images'
                sh 'docker network ls'
                sh 'rm -rf it/target || true'
            }
        }
        stage('Integration Test') {
            steps {
                sh "sbt -mem 40960 clean;it/test"
            }
            post {
                always {
                    dir('it/target/logs') {
                        sh "tar zcf \"${env.BUILD_TAG}.logs.tar.gz\" * || :"
                    }
                    dir('node-it/target/logs') {
                        sh "tar zcf \"${env.BUILD_TAG}.node.logs.tar.gz\" * || :"
                    }
                }
            }
        }
    }
    post {
        always {
            archiveArtifacts artifacts: 'it/target/logs/*.logs.tar.gz'
        }
    }
}
