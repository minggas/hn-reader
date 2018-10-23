# Hacker News Reader

### My version of the Hacker News website made with Elm

## Getting Started

### Dependencies

- #### Elm (To instal Elm follow this [guide](https://guide.elm-lang.org/install.html))

### Setup

- #### Clone the project

      $ git clone https://github.com/minggas/hn-reader.git

- #### Enter the project folder

      $ cd hn-reader

### Running

- #### Develop Mode

  - Run the command

        $ elm reactor

    This starts a server at http://localhost:8000. You can navigate to the Elm file called Main.elm and click it to run.

    Note: The app will run without any style, still need to know how to run an external CSS file with elm reactor

- #### Production Mode

  - Enter in the elm folder

    $ cd elm

  - Run the command

    $ elm make src/Main.elm --optimize --output=../main.js

  This put on the base folder the Elm app compile in a optimize JS file. Just put the index.html, main.js and style.css files on your host to make it work.
