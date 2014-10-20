# Collab

A screen sharing tool that allows you to share source code in real time to the 
web.

[![Build Status](https://travis-ci.org/dennis84/collab-haskell.svg?branch=master)](https://travis-ci.org/dennis84/collab-haskell)

## API

    > -
    < join@SENDER_ID{"id":"SENDER_ID"}

    > -
    < leave@SENDER_ID{"id":"SENDER_ID"}

    > members
    < members@SENDER_ID[{"id":"ID","name":"NAME","me":true}]

    > code{"content":"", "file":"", "lang":"haskell"}
    < code@SENDER_ID{"content":"", "file":"Main.hs", "lang":"haskell"}

    > cursor{"x":1,"y":1,"file":"Main.hs"}
    < cursor@SENDER_ID{"x":1,"y":1,"file":"Main.hs"}

    > change-nick{"name":"NAME"}
    < change-nick@SENDER_ID{"id":"SENDER_ID","name":"NAME"}
