# rpki-pub-server

Implementation of publishing server for publishing RPKI cryptographic material.

Some links:

https://tools.ietf.org/html/draft-ietf-sidr-publication-05

http://tools.ietf.org/id/draft-tbruijnzeels-sidr-delta-protocol-03.txt

http://datatracker.ietf.org/wg/sidr/charter/

Current TODOs:


* Generate notification.xml

* Add repo reading from the specified location
* Add updating files on disk
  - do it atomically (with temporary directory?)

* Add some QuickCheck test for 
  msg (pub, withdraw) + repo == snapshot + pub - withdraw
