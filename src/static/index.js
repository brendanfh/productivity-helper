// pull in desired CSS/SASS files
require('./styles/main.scss');
var $ = jQuery = require('../../node_modules/jquery/dist/jquery.js');           // <--- remove if jQuery not needed
require("./sounds/chime.mp3");
// require('../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js');   // <--- remove if Bootstrap's JS not needed 

// inject bundled Elm app into div#main
var Elm = require('../elm/Main');
var app = Elm.Main.embed(document.getElementById('main'));

app.ports.saveToLocalStorage.subscribe(function (data) {
    localStorage.setItem("goals", JSON.stringify({ tasks: data }));
});

app.ports.loadFromLocalStorage.subscribe(function (_) {
    var t = JSON.parse(localStorage.getItem("goals"));
    if (t == null) {
        app.ports.localStorageSubscription.send([]);
        return;
    }
    if (t.tasks != null) {
        var tasks = t.tasks;
        app.ports.localStorageSubscription.send(tasks);
        return;
    }
});

app.ports.playChime.subscribe(function (_) {
    var sound = new Audio("static/sounds/chime.mp3");
    sound.play();
});