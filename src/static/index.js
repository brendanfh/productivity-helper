require('./styles/main.scss');
require("./sounds/chime.mp3");

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