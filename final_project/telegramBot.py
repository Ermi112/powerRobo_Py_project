import telepot
import requests
from datetime import datetime

# Define the function to handle incoming messages
def handle(msg):
    try:
        content_type, chat_type, chat_id = telepot.glance(msg)
    except Exception as e:
        print(f"Error occurred while trying to handle incoming message: {e}")
        return

    if content_type == 'text':
        command = msg['text'].strip().lower()

        if command == '/start':
            message = "Hello! Welcome to my bot. Here are the available commands:\n/start - Start the bot\n/help - Show this help message\n/info - Show information about the bot\n/status - Show the current status of the bot\n/time - Show the current time\n/weather [location] - Show the weather for a location"
            bot.sendMessage(chat_id, message, parse_mode='Markdown')

            # Send a welcome image with a greeting message
            image_url = 'https://png.pngtree.com/png-vector/20190221/ourlarge/pngtree-colorful-welcome-poster-with-brush-stroke-png-image_691434.jpg' # replace with your image url
            caption = 'Welcome to my bot! I hope you enjoy using it.'
            bot.sendPhoto(chat_id, image_url, caption)
        elif command == '/help':
            message = "These are the available commands:\n/start - Start the bot\n/help - Show this help message\n/info - Show information about the bot\n/status - Show the current status of the bot\n/time - Show the current time\n/weather [location] - Show the weather for a location"
            bot.sendMessage(chat_id, message, parse_mode='Markdown')
        elif command == '/info':
            message = "I am chat bot created by wubtBot. It can respond to several commands."
            bot.sendMessage(chat_id, message)
        elif command == '/status':
            bot.sendMessage(chat_id, "The bot is currently running.")
        elif command == '/time':
            now = datetime.now()
            message = f"The current time is {now.strftime('%H:%M:%S')}"
            bot.sendMessage(chat_id, message)
        elif command.startswith('/weather'):
            if len(command.split()) < 2:
                bot.sendMessage(chat_id, "Please specify a location after /weather.")
            else:
                location = command.split(' ', 1)[1]

                # Replace YOUR_API_KEY with your actual API key
                url = f"https://api.openweathermap.org/data/2.5/weather?q={location}&appid= c105f0fbb27330540545ded1755d55be"

                # Make a GET request to the OpenWeatherMap API
                response = requests.get(url)

                if response.status_code == 200:
                    # Parse the JSON response
                    data = response.json()

                    # Extract the relevant weather information
                    temperature = data['main']['temp']
                    description = data['weather'][0]['description']
                    humidity = data['main']['humidity']

                    # Construct the weather message
                    message = f"The temperature in {location} is {temperature:.1f}°F with {description} and humidity of {humidity}%."

                    # Send the weather message to the user
                    bot.sendMessage(chat_id, message)
                else:
                    bot.sendMessage(chat_id, "Sorry, I couldn't retrieve the weather information for that location.")
        else:
            bot.sendMessage(chat_id, "Sorry, I didn't understand that command.")


# Define the token for your bot obtained from BotFather
TOKEN = '6151729659:AAFpbZso50U1wnZsVdtSG3mf9g_AkXRnwYk'

# Create an instance of the bot
bot = telepot.Bot(TOKEN)

# Start listening for incoming messages
bot.message_loop(handle)

# Keep the program running
while True:
    pass
