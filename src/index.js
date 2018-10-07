var firebase = require("firebase/app");
require("firebase/auth");
require("firebase/database");
require("firebase/firestore");
require("firebase/messaging");
require("firebase/functions");

// Initialize Firebase
var config = {
  apiKey: "AIzaSyD9Yd-QTn1zTuF8Sx_xTR145yVCIGXCUEQ",
  authDomain: "elm-todo-063-jp.firebaseapp.com",
  databaseURL: "https://elm-todo-063-jp.firebaseio.com",
  projectId: "elm-todo-063-jp",
  storageBucket: "elm-todo-063-jp.appspot.com",
  messagingSenderId: "946848464130"
};
firebase.initializeApp(config);

// Get a reference to the database service
var database = firebase.database();

var provider = new firebase.auth.GithubAuthProvider();

var user

require('./main.scss');

const Elm = require('./Main.elm');
const mountNode = document.getElementById('main');

const app = Elm.Elm.Main.init({
  node: document.getElementById('main')
});

firebase.auth().signInWithPopup(provider).then(function(result) {
  // This gives you a GitHub Access Token. You can use it to access the GitHub API.
  var token = result.credential.accessToken;
  // The signed-in user info.
  user = result.user;
  // ...
  var datastore = firebase.database().ref('data/' + user.uid);
  datastore.on('value', function(snapshot) {
    console.log("snapshot: "+snapshot.val())
    app.ports.getUpdate.send(snapshot.val())
  });
}).catch(function(error) {
  // Handle Errors here.
  var errorCode = error.code;
  var errorMessage = error.message;
  // The email of the user's account used.
  var email = error.email;
  // The firebase.auth.AuthCredential type that was used.
  var credential = error.credential;
  // ...
});

// Elm -> JS
function writeData(data) {
  if(user){
    firebase.database().ref('data/' + user.uid).set(data);
  }else{
    console.log("Please Login")
  }
}

app.ports.sendUpdate.subscribe(function(str) {
  console.log("json: "+str);
  writeData(str)
});
