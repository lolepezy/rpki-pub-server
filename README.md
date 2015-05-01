# rpki-pub-server

Implementation of publishing server for publishing RPKI cryptographic material.

Some links:

https://tools.ietf.org/html/draft-ietf-sidr-publication-05

http://tools.ietf.org/id/draft-tbruijnzeels-sidr-delta-protocol-03.txt

http://datatracker.ietf.org/wg/sidr/charter/

Current TODOs:


* Add some QuickCheck test for
  msg (pub, withdraw) + repo == snapshot + pub - withdraw


To launch a test server in the Docker image issue in the project directory:

### Install docker if needed
brew install boot2docker
boot2docker init
boot2docker up
$(boot2docker shellinit)

set the environment variables it suggests you to set and run

docker build --tag=rpki/pub-server.0.1 .
docker run --publish=9999:9999 rpki/pub-server.0.1

It will run the test server image, listening on the port 9999,
so it's possible to use it as something like

http://192.168.59.103:9999/notification.xml

The exact IP will be printed by "boot2docker up" command.
