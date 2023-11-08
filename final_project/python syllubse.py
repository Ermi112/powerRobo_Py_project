import requests
from bs4 import BeautifulSoup

# Send a GET request to the URL
url = 'https://set.jainuniversity.ac.in/academics/electronics-and-communications/btech-robotics-automation'
response = requests.get(url)

# Parse the HTML content using BeautifulSoup
soup = BeautifulSoup(response.content, 'html.parser')

# Find the div element with class "tab-pane"
syllabus_div = soup.find('div', {'class': 'tab-pane'})

# Find the unordered list elements containing the syllabus items
syllabus_items = syllabus_div.find_all('ul')

# Find the heading elements containing the semester titles
semester_headings = syllabus_div.find_all('h3')

# Print out the subjects for each semester
for i, semester_items in enumerate(syllabus_items):
    semester_title = semester_headings[i].text.strip()
    print(f"{semester_title}:")
    for item in semester_items.find_all('li'):
        print(item.text.strip())
    print()
