# hackathon-tellchima

Make Chima New Again, with a competition

## Format

1. Everyone develops their own application in their own directory respectively.
2. Make sure you have at least a README and instructions on how to start your
   application.
3. It is not required to deploy your application, as long as the slash command
   works in the demo.
   - Of course you would also deploy it to our staging k8s / leverage service
     like [Render](https://render.com/).
4. Demo time is TBC. Expect some time around the 1st week of working after
   Christmas holidays.

## Getting started

### Creating slash command

For dev purpose, everyone should create their own slash command for testing. We
follow this guide:

- https://api.slack.com/interactivity/slash-commands#creating_commands

With the following information:

1. We already have an app created and connected to Oursky workspace: https://api.slack.com/apps/A04HKMSH3DE
2. Create your slash command here: https://api.slack.com/apps/A04HKMSH3DE/slash-commands
   1. You can be creative on the slash command name as long as it is distinguishable
      from the other. I would recommend the following format to avoid populating
      the workspace: `tell-[something]`, where `something` can be anything. e.g.
      your slack username or whatever.
3. You could use a web server to verify the webhook before really writing your
   own server, which could be created rather easily with service like
   https://requestbin.com/, or with a local echo server like
   https://github.com/jmalloc/echo-server.

### Sending messages to channel

For dev purpose, the slack app has been added to `#p-tellchima` already. You could
test message with the following curl:

```sh
> curl \
    -H 'Authorization: Bearer OAUTH_TOKEN' \
    -H 'Content-type: application/json' \
    -d '{"channel": "C04GPBBHRM4", "text": "I_FORGOT_TO_REPLACE_THE_TEST_MESSAGE"}' \
    https://slack.com/api/chat.postMessage
```

- `OAUTH_TOKEN` could be found here: https://api.slack.com/apps/A04HKMSH3DE/oauth
- `I_FORGOT_TO_REPLACE_THE_TEST_MESSAGE`: Replace with your own test message

Ref: https://api.slack.com/messaging/sending#composing
