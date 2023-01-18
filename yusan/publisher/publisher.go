package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
)

type Context struct {
	ApiClient                    *http.Client
	ChimaListenerEndpoint        string
	HackerNewsTopStoriesEndpoint string
	SlackBotOauthToken           string
	SlackChannelId               string
	SlackPostMessageEndpoint     string
}

func env(required bool, key string) string {
	value := os.Getenv(key)
	if required && value == "" {
		fmt.Printf("Missing env var '%s'\n", key)
		os.Exit(1)
	}
	return value
}

func getContext() *Context {
	return &Context{
		ApiClient:                    &http.Client{},
		ChimaListenerEndpoint:        env(true, "CHIMA_LISTENER_ENDPOINT"),
		HackerNewsTopStoriesEndpoint: env(true, "HACKER_NEWS_TOP_STORIES_ENDPOINT"),
		SlackBotOauthToken:           env(true, "SLACK_BOT_OAUTH_TOKEN"),
		SlackChannelId:               env(true, "SLACK_CHANNEL_ID"),
		SlackPostMessageEndpoint:     env(true, "SLACK_POST_MESSAGE_ENDPOINT"),
	}
}

func getHackerNewsTopStoryUrl(context *Context) string {
	req, err := http.NewRequest("GET", context.HackerNewsTopStoriesEndpoint, nil)
	if err != nil {
		panic(err)
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := context.ApiClient.Do(req)
	if err != nil {
		panic(err)
	}
	defer res.Body.Close()

	resBody, err := io.ReadAll(res.Body)
	if err != nil {
		panic(err)
	}

	var stories []int
	if err := json.Unmarshal(resBody, &stories); err != nil {
		fmt.Println("Cannot parse hacker news top stories!")
		panic(err)
	}

	return fmt.Sprintf("https://news.ycombinator.com/item?id=%d", stories[0])

}

type Message struct {
	Id   int    `json:"id"`
	Body string `json:"body"`
}

func collect(context *Context) *[]Message {

	req, err := http.NewRequest("POST", context.ChimaListenerEndpoint, nil)
	if err != nil {
		panic(err)
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := context.ApiClient.Do(req)
	if err != nil {
		panic(err)
	}
	defer res.Body.Close()

	resBody, err := io.ReadAll(res.Body)
	if err != nil {
		panic(err)
	}

	var messages []Message
	if err := json.Unmarshal(resBody, &messages); err != nil {
		fmt.Println("Cannot parse messages!")
		panic(err)
	}

	return &messages
}

type SlackPostMessageRequestBody struct {
	Channel string `json:"channel"`
	Text    string `json:"text"`
}

func post(context *Context, text string) {
	b, err := json.Marshal(&SlackPostMessageRequestBody{
		Channel: context.SlackChannelId,
		Text:    text,
	})
	if err != nil {
		panic(err)
	}

	reqBody := bytes.NewBuffer(b)
	req, err := http.NewRequest("POST", context.SlackPostMessageEndpoint, reqBody)
	if err != nil {
		panic(err)
	}
	req.Header.Add("Authorization", fmt.Sprintf("Bearer %s", context.SlackBotOauthToken))
	req.Header.Add("Content-Type", "application/json")

	res, err := context.ApiClient.Do(req)
	if err != nil {
		panic(err)
	}

	_ = res
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("No subcommand!")
		os.Exit(1)
	}
	context := getContext()
	switch os.Args[1] {
	case "4pm":
		post(context, "If you have something to post, please `/tellchima`. Publish daily at 5pm.")
	case "5pm":
		post(context, "Chima Summary (`/tellchima` to add)")
		messages := collect(context)
		if len(*messages) > 0 {
			for _, message := range *messages {
				post(context, fmt.Sprintf("`#%04d` %s", message.Id%10000, message.Body))
			}
		} else {
			hackerNewsTopStoryUrl := getHackerNewsTopStoryUrl(context)
			post(context, fmt.Sprintf("No News. But here's the top HN story: %s", hackerNewsTopStoryUrl))
		}
	default:
		fmt.Println("Unknown subcommand!")
		os.Exit(1)
	}
}
