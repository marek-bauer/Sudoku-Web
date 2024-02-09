# Sudoku Web app

Goal of this project was to prepare frontend only application for playing and
solving sudoku puzzles. The application therefore can be easily distributed to
end users without need of maintaining sever, or with minimal servers requirements
that serve only static files.

## Used technologies

My personal goal, that inspired me to continue with this project was goal of learning 
purescript language. Unfortunately, purescript compiled to java script is a too slow 
tool for such task, therefore I prepared web assembly module that will perform the 
heavy lifting of soling sudoku puzzles. 

## How to run this application

In order to run to run this app you first need to install node.js package together 
with npm package manager. Later enter:
* `npm install` - for installing dependencies 
* `npm run build-all` - to build all code

If you want you can serve the app locally using `npm run serve`

You can also build android application based on this project, providing you have installed 
Android SDK. In order to achieve that you should enter:
* `npm run add-android` - to prepare android dependencies 
* `npm run build-cordova` - to build application