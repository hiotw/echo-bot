# Echo bot

Just an echo bot.

# Build

1. Clone the repository:

        git clone https://github.com/hiotw/echo-bot.git

2. Move to created directory and build:

        cd echo-bot
        stack build

# Usage

Run:

    stack exec echo-bot-exe <path to config file>

Config file is a JSON file in format:

    {
        "type": "telegram",
        "token": "your telegram token"
    }
