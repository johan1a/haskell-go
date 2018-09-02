def label = "worker-${UUID.randomUUID().toString()}"

podTemplate(label: label,
  serviceAccount: 'jenkins-master',
  containers: [
      containerTemplate(name: 'docker', image: 'docker', command: 'cat', ttyEnabled: true),
],
volumes: [
  hostPathVolume(mountPath: '/var/run/docker.sock', hostPath: '/var/run/docker.sock')
]) {
  node(label) {
    def myRepo = checkout scm
    def gitCommit = myRepo.GIT_COMMIT

    stage('Build and push Docker images') {
      container('docker') {
      withCredentials([[$class: 'UsernamePasswordMultiBinding',
                           credentialsId: 'docker-hub-credentials',
                           usernameVariable: 'DOCKER_HUB_USER',
                           passwordVariable: 'DOCKER_HUB_PASSWORD']]) {
          sh """
            docker login -u ${DOCKER_HUB_USER} -p ${DOCKER_HUB_PASSWORD}
            docker build -t johan1a/haskell-go:${gitCommit} .
            docker build -t johan1a/haskell-go:latest .
            docker push johan1a/haskell-go:${gitCommit}
            docker push johan1a/haskell-go:latest
            """
        }
      }
    }
  }
}
