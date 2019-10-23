#!/usr/bin/env groovy

/*
This is a Jenkins scripted pipeline to launch integration tests. We use following plugins:
- Extended Choice parameter: https://plugins.jenkins.io/extended-choice-parameter
- Generic Webhook Trigger plugin: https://wiki.jenkins.io/display/JENKINS/Generic+Webhook+Trigger+Plugin

We alse need to

On the GitHub side navigate to Repository Settings > Webhooks >
- Content type: application/json
- SSL Verification: enabled
- Events: choose the events you want to trigger build on
- Payload URL: https://<jenkins_web_address>/generic-webhook-trigger/invoke?token=wavesPipelineTriggerToken

- generate a personal access token with 'repo' permissions add it to Jenkins secrets and specify ID to 'githubPersonalToken' variable

To set up pipeline in Jenkins: New Item > Pipeline > name it > OK > Scroll to Pipeline pane >
- Definition: Pipeline script from SCM, SCM: Git, Repo: 'https://github.com/wavesplatform/Waves.git',
- Lightweight checkout: disabled. Save settings and launch pipeline
*/

@Library('jenkins-shared-lib')

import jenkins.model.Jenkins.*
import devops.waves.*
ut = new utils()
scripts = new scripts()
def repoUrl = 'https://github.com/wavesplatform/Waves.git'
def branch = false
def headCommitMessage = false
def gitCommit = false
def pullRequestNumber = false
def testTasks = [:]
def prInfo = [:]
def githubRepo = 'wavesplatform/Waves'
def githubPersonalToken = 'waves-github-token'
def jenkinsCreds = 'jenkins-jenkins-creds'
def pipelineStatus = ['unitTests': false, 'integrationTests': false]
def logUrls = ['unitTests': false, 'integrationTests': false ]
def testResults = ['unitTests': false, 'integrationTests': false ]
def releaseBranchNotify = false
def jdkVersion = 'openjdk11'
def sbtVersion = 'sbt-1.2.8'
def slackChannel = "jenkins-notifications"
properties([
    ut.buildDiscarderPropertyObject('14', '30'),
    parameters([
        ut.wHideParameterDefinitionObject('pr_from_ref'),
        ut.wHideParameterDefinitionObject('head_commit_message'),
        ut.wHideParameterDefinitionObject('pull_request_number'),
        ut.choiceParameterObject('branch', getGitBranchesScript(repoUrl), 'PARAMETER_TYPE_SINGLE_SELECT', Boolean.TRUE)
    ]),

    pipelineTriggers([
        [$class: 'GenericTrigger',
        genericVariables: [
            [ key: 'branch', value: '$.ref', regexpFilter: 'refs/heads/', defaultValue: '' ],
            [ key: 'pr_action', value: '$.action'],
            [ key: 'head_commit_message', value: '$.head_commit.message'],
            [ key: 'deleted', value: '$.deleted'],
            [ key: 'pr_from_ref', value: '$.pull_request.head.ref' ],
            [ key: 'pull_request_number', value: '$.pull_request.number' ]],
        // this is a place where some magic occurs ;)
        regexpFilterText: '$deleted$branch$pr_action',
        regexpFilterExpression: 'falsemaster|falseversion-0.+|opened|reopened|synchronize',
        causeString: "Triggered by GitHub Webhook",
        printContributedVariables: true,
        printPostContent: true,
        token: 'wavesPipelineTriggerToken' ]
    ])
])

stage('Aborting this build'){

    // Here we check if parameter 'branch' does not have any assigned value or it's value
    // is a default one -- '-- Failed to retrieve any data---'
    // In this case we won't proceed
    if (params.branch && params.branch.length() && ! params.branch.contains('--')){
        branch = params.branch
    }

    // If this is a hook with a GitHub pull request event, then we take 'branch'
    // from params.pr_from_ref
    if (params.pr_from_ref && params.pr_from_ref.length()){
        branch = params.pr_from_ref
    } else if (branch.contains('master') || branch.contains('version-0')){
        releaseBranchNotify = true
    }

    if (params.head_commit_message && params.head_commit_message.length()){
        headCommitMessage = params.head_commit_message
    }
    if (params.pull_request_number && params.pull_request_number.length()){
        pullRequestNumber = params.pull_request_number
    }

    if (! branch) {
        echo "Aborting this build. Variable 'branch' not defined. We can't proceed since the git branch is uknown..."
        currentBuild.result = Constants.PIPELINE_ABORTED
        return
    }
    else
        echo "Parameters specified: ${params}"
}

if (currentBuild.result == Constants.PIPELINE_ABORTED){
    return
}

timeout(time:90, unit:'MINUTES') {
    node('integrationTests'){
        currentBuild.result = Constants.PIPELINE_SUCCESS
        timestamps {
            wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
                try {
                    currentBuild.displayName = "#${env.BUILD_NUMBER} - ${branch}"
                    stage('Checkout') {
                        env.branch=branch
                        sh 'env'
                        step([$class: 'WsCleanup'])
                        gitCheckout(branch: branch, url: repoUrl)
                        stash name: 'sources', includes: '**'
                        gitCommit = ut.shWithOutput("git rev-parse HEAD")
                        if (releaseBranchNotify){
                            if (! headCommitMessage){
                                headCommitMessage = ut.shWithOutput('git log -1 --pretty=%B')
                            }

                            if(headCommitMessage.toLowerCase().contains('node') || branch.toLowerCase() =~ /^node.+/){
                                releaseBranchNotify = 'node'
                            } else if (headCommitMessage.toLowerCase().contains('sc') || branch.toLowerCase() =~ /^sc.+/){
                                releaseBranchNotify = 'sc'
                            } else{
                                releaseBranchNotify = 'all'
                            }

                            if (headCommitMessage.replaceAll("[^0-9]", "").length() == 4){
                                pullRequestNumber = headCommitMessage.replaceAll("[^0-9]", "")
                            }
                        }

                        if (pullRequestNumber){
                            prInfo = ut.getGitHubPullRequestInfo(pullRequestNumber, githubRepo, githubPersonalToken)
                        }
                    }

                    testTasks['Get Stages Urls'] = {
                        sleep 30
                        logUrls['unitTests']=ut.getStepLogUrl('Unit Tests', 'checkPR', jenkinsCreds)
                        logUrls['integrationTests']= ut.getStepLogUrl('Integration Tests', 'it/test', jenkinsCreds)
                        ut.setGitHubBuildStatus(githubRepo, githubPersonalToken, gitCommit, 'Jenkins Unit Tests', logUrls['unitTests'])
                        ut.setGitHubBuildStatus(githubRepo, githubPersonalToken, gitCommit, 'Jenkins Integration Tests', logUrls['integrationTests'])
                    }

                    testTasks['Unit Tests'] = {
                        node('integrationTests'){
                            stage('Unit Tests') {
                                step([$class: 'WsCleanup'])
                                unstash 'sources'
                                env.branch=branch
                                ut.sbtPreconditions(jdkVersion, sbtVersion, '-Xmx3g -Xms3g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled')
                                try{
                                    sh "SBT_THREAD_NUMBER=7 sbt \";update;clean;coverage;checkPR;coverageReport\""
                                    pipelineStatus['unitTests'] = true
                                }
                                finally{
                                    testResults['unitTests'] = (pipelineStatus['unitTests']) ? 'success' : 'failure'
                                    ut.setGitHubBuildStatus(githubRepo, githubPersonalToken, gitCommit, 'Jenkins Unit Tests', logUrls['unitTests'], testResults['unitTests'])
                                    sh "tar -czvf unit-test-reports.tar.gz -C target/test-reports/ . || true"
                                    junit allowEmptyResults: true, keepLongStdio: true, testResults: 'target/test-reports/*.xml'
                                    stash name: 'test-reports', allowEmpty: true, includes: 'unit-test-reports.tar.gz'
                                }
                            }
                        }
                    }
                    testTasks['Integration Tests'] = {
                        node('integrationTests'){
                            stage('Integration Tests') {
                                step([$class: 'WsCleanup'])
                                unstash 'sources'
                                env.branch=branch
                                sh """
                                    ps -aux | grep '[s]bt' || find ~/.ivy2/ -name '*SNAPSHOT*' -exec rm -rfv {} \\; || true
                                    ps -aux | grep '[s]bt ;update;clean;it/test' ||
                                    {
                                        docker rm -f \$(docker ps -a | grep waves | grep node | awk '{ print \$1 }') || true
                                        docker rmi -f \$(docker images | grep node | awk '{ print \$3 }') || true
                                        docker volume prune || true
                                        docker ps -a
                                        docker images
                                        docker network ls ;
                                    }
                                """
                                ut.sbtPreconditions(jdkVersion, sbtVersion, '-Xmx3g -Xms3g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled')
                                try{
                                    sh "SBT_THREAD_NUMBER=7 JENKINS_JOB_ID=jenkins-${BUILD_ID} sbt \";update;clean;it/test\""
                                    pipelineStatus['integrationTests'] = true
                                }
                                finally{
                                    sh "docker kill \$(docker ps -a | grep jenkins-${BUILD_ID} | awk '{ print \$1 }') || true"
                                    sh "docker rm -f \$(docker ps -a | grep jenkins-${BUILD_ID} | awk '{ print \$1 }') || true"
                                    testResults['integrationTests'] =  (pipelineStatus['integrationTests']) ? 'success' : 'failure'
                                    ut.setGitHubBuildStatus(githubRepo, githubPersonalToken, gitCommit, 'Jenkins Integration Tests', logUrls['integrationTests'], testResults['integrationTests'])
                                    sh "tar -czvf it-logs.tar.gz -C node-it/target/logs/ . || true"
                                    sh "tar -czvf it-test-reports.tar.gz -C target/test-reports/ . || true"
                                    junit allowEmptyResults: true, keepLongStdio: true, testResults: 'target/test-reports/*.xml'
                                    stash name: 'it-logs', allowEmpty: true, includes: 'node-logs.tar.gz, it-test-reports.tar.gz'
                                }
                            }
                        }
                    }
                    // this option below controls if we fail the whole job in case any of the parallel steps fail
                    // testTasks.failFast = true
                    parallel testTasks
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
                    if (prInfo ||  releaseBranchNotify){
                        ut.sendNotifications(prInfo, testResults, logUrls, branch, releaseBranchNotify)
                    }
                    slackIt(channel: slackChannel, buildStatus:currentBuild.result)
                    unstash 'it-logs'
                    unstash 'test-reports'
                    archiveArtifacts artifacts: 'it-logs.tar.gz, unit-test-reports.tar.gz, it-test-reports.tar.gz'
                }
            }
        }
    }
}
