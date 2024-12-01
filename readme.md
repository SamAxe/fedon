---
title:          readme
subtitle:
author:
keywords:
created:        2024/12/01 14:26
Last modified:  2024/12/01 14:26
abstract-title: readme
abstract: |
   (auto generated line)
...



Current status is that the server can serve pages and seems to do
simple basic Fedwiki editing operations, but doesn't support any
plugins.

The factories.json, site-index.json, and sitemap.json are currently all static and not updated.


Consider: If everything on the server is encrypted, then the encryption has to happen on the client and
all the services that use meta information has to be done on the client.  If there is a hybrid approach
and the server has some metadata, then those services could be done on the server...food for thought.

Consider: Support mostly offline servers.  For example, a server might have a hotspot on a boat so that
local devices can work off the server, but the server would be off the internet except for when it was
at port.

Consider: Supporting collaborative writing features and the lifecycle of written thought.  There is the
"memmex" type brain that captures the "writing to think" type aspect and then the publication process that
captures the "writing for an audience" type process.

Consider: Private by default.  That is, pages are only accessible to the site-owner and not available
to the public or to web crawlers.  This extends the "pull only" and "only site owner can write" type
philosophy towards more individual control.
There could be several reasons for desiring this way:
 - You pay for internet traffic/cpu time, e.g. on hosted sites.  Getting pinged, even to say nothing still
   incurs CPU cost.
 - Want to reign in anonymous stalkers.

Crazy idea: Make a fedwiki LSP so I can use my editor to view/edit pages.

Next steps, in no particular order

- [ ] factor out the client part of this server from the page store part of the server
- [ ] Make a v2 protocol for basic edit operations
  * Contrast the implementation of things like rendering the story from the journal and the route handling with a v2 vs v1 protocol.
  * Ensure that v1 protocol can be nudged to a v2 system and ideally, that v2 could still be interoperable with v1 clients.
- [ ] Consider Friends of Friends and other security oriented aspects.
- [ ] Consider the system/* files and whether they should be served, be dynamic etc
- [ ] Integrate more logging, system administration, and protection for being on the big bad internet, e.g. automatic blacklist or whitelist type options.
- [ ] A version 2 client (maybe typescript, maybe something else) with much better markdown support...
