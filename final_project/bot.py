import telepot
from telepot.loop import MessageLoop

# create a Telegram bot instance using your bot token
bot = telepot.Bot('5700079324:AAEAHNBR2FQ4yaU4S4oDBy8k-yC5jFcszNw')

# define a function to handle the /start command
def start(chat_id):
    bot.sendMessage(chat_id, 'Welcome to the customer service bot!')

# define a function to handle the /help command
def help(chat_id):
    bot.sendMessage(chat_id, 'Available commands: /start, /help, /info, /status, /time, /other')

# define a function to handle the /info command
def info(chat_id):
    bot.sendMessage(chat_id, 'This is a customer service bot that can help you with your inquiries.')

# define a function to handle the /status command
def status(chat_id):
    bot.sendMessage(chat_id, 'The customer service is currently available.')

# define a function to handle the /time command
def time(chat_id):
    import datetime
    now = datetime.datetime.now()
    bot.sendMessage(chat_id, 'The current time is ' + now.strftime("%H:%M:%S"))

# define a function to handle the /other command
def other(chat_id):
    bot.sendMessage(chat_id, "Sorry, I don't understand that command.")

# define a function to handle incoming messages
def handle_message(msg):
    content_type, chat_type, chat_id = telepot.glance(msg)
    if content_type == 'text':
        if msg['text'] == '/start':
            start(chat_id)
        elif msg['text'] == '/help':
            help(chat_id)
        elif msg['text'] == '/info':
            info(chat_id)
        elif msg['text'] == '/status':
            status(chat_id)
        elif msg['text'] == '/time':
            time(chat_id)
        else:
            other(chat_id)

# start the message loop
MessageLoop(bot, handle_message).run_forever()
