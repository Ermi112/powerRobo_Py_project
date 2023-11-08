import telepot
import requests

# Initialize the bot object with your Telegram Bot API token
bot = telepot.Bot('6151729659:AAFpbZso50U1wnZsVdtSG3mf9g_AkXRnwYk')

# Define the function to handle incoming messages
def handle(msg):
    content_type, chat_type, chat_id = telepot.glance(msg)
    command = msg['text']

    # Add the /weather command to the handle() function
    if command.startswith('/weather'):
        # Get the location from the user's command
        location = command.split(' ')[1]

        # Call the weather() function with the location parameter
        weather(chat_id, location)

# Define the function to handle the /weather command
def weather(chat_id, location):
    # Replace YOUR_API_KEY with your actual API key
    url = f"https://api.openweathermap.org/data/2.5/weather?q={location}&appid=5b712eabfff704b9ddc53cc3b4ddddd5"

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

# Start the bot
bot.message_loop(handle)

# Keep the program running
while True:
    pass

