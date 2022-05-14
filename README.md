# Quick Toot
Quick toot is an emacs plugin that lets you post toots to any oauth-supporting mastodon
instance. It is not a full fledged mastodon client that lets you read notifications and
timelines, because while it pains to admit it, browsers are just better at that
stuff. What browsers aren't good at though, is editing text. That's where quick toot comes
in. Doing some work in emacs and want to post a quick toot to mastodon? Run `M-x qtoot`
and start typing right away. `C-c C-c` will post the toot in the current buffer to an
instance of your choice.

## Features
Below is a list of planned/completed features

| Feature                                   | Completed? |
|-------------------------------------------|------------|
| Oauth login                               | ✔          |
| Basic toot                                | ✔          |
| Toot with Content Warning                 | ✔          |
| Scheduled toot                            | ❌         |
| Reply to another toot                     | ❌         |
| Sending a DM                              | ❌         |
| Reading DM's (text only)                  | ❌         |
| Reading replies to your toots (text only) | ❌         |

Oauth login is done through a server application that handles the redirects that are part
of the oauth protocol. The server runs at https://auth.qtoot.hugot.nl . The application
is completely open source and its source code can be found at
    https://git.snorba.art/hugo/generic-mastodon-authenticator .

## Screenshot
![Screenshot of qtoot](https://cdn.hugot.nl/random/hahNg6eis9gooTae/Screenshot_2019-12-03_11-14-39.png)

## Install
Put qtoot.el somewhere in your load-path and require it. That's all :)
