node {

  environment {
    SYNCER_HOSTS = 'archlinux'
  }
  stage('Clone repository') {
    checkout scm
  }

  def image

  stage('Build Docker image') {
    image = docker.build("johan1a/haskell-go:${env.BUILD_ID}")
  }

}
