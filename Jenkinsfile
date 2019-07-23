#!/usr/bin/env groovy

@Library('jenkins-shared-lib')
import devops.waves.*
ut = new utils()
def buildTasks = [:]
def repo_url = 'https://github.com/wavesplatform/Waves.git'

properties([
    parameters([
        listGitBranches(
            branchFilter: 'origin/(.*)',
            credentialsId: '',
            defaultValue: '',
            name: 'branch',
            listSize: '20',
            quickFilterEnabled: false,
            remoteURL: repo_url,
            selectedValue: 'NONE',
            sortMode: 'DESCENDING_SMART',
            type: 'PT_BRANCH')])
])

stage('Aborting this build'){
    // On the first launch pipeline doesn't have any parameters configured and must skip all the steps
    if (env.BUILD_NUMBER == '1'){
        echo "This is the first run of the pipeline! It is now should be configured and ready to go!"
        currentBuild.result = Constants.PIPELINE_ABORTED
        return
    }
    if (! branch ) {
        echo "Aborting this build. Please run it again with the required parameters specified."
        currentBuild.result = Constants.PIPELINE_ABORTED
        return
    }
    else
        echo "Parameters are specified. Branch: ${branch}"
}

if (currentBuild.result == Constants.PIPELINE_ABORTED){
    return
}

timeout(time:90, unit:'MINUTES') {
    node('wavesnode'){
        currentBuild.result = Constants.PIPELINE_SUCCESS
        timestamps {
            wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
                try {
                    withEnv(["SBT_THREAD_NUMBER=7"]) {

                        currentBuild.displayName = "#${env.BUILD_NUMBER} - ${branch}"

                        stage('Checkout') {
                            sh 'env'
                            step([$class: 'WsCleanup'])
                            checkout([
                                $class: 'GitSCM',
                                branches: [[ name: branch ]],
                                doGenerateSubmoduleConfigurations: false,
                                extensions: [],
                                submoduleCfg: [],
                                userRemoteConfigs: [[url: repo_url]]
                            ])
                        }

                        stage('Unit Test') {
                            ut.sbt '-mem 10240 checkPR'
                        }

                        stage('Check containers') {
                            sh 'docker rmi com.wavesplatform/it com.wavesplatform/node-it com.wavesplatform/dex-it || true'
                            sh 'docker ps -a'
                            sh 'docker images'
                            sh 'docker network ls'
                            sh 'rm -rf it/target || true'
                        }

                        stage('Integration Test') {
                            try {
                                ut.sbt '-mem 40960 clean it/test'
                            }
                            catch (err) {}
                            finally{
                                dir('it/target/logs') {
                                    sh "tar zcf logs.tar.gz * || true"
                                }
                                dir('node-it/target/logs') {
                                    sh "tar zcf node.logs.tar.gz * || true"
                                }
                            }
                        }

                        stage('Docker cleanup') {
                            sh "docker system prune -af --volumes"
                        }
                    }
                }
                catch (err) {
                    currentBuild.result = Constants.PIPELINE_FAILURE
                    println("ERROR caught")
                    println(err)
                    println(err.getMessage())
                    println(err.getStackTrace())
                    println(err.getCause())
                    println(err.getLocalizedMessage())
                    println(err.toString())
                 }
                finally{
                    archiveArtifacts artifacts: 'it/target/logs/*.logs.tar.gz'
                    ut.notifySlack("mtuktarov-test", currentBuild.result)
                }
            }
        }
    }
}
