import { Elm } from "./src/Main.elm"
import "./index.html"

const app = Elm.Main.init({
  node: document.getElementById("elm-node")
})

const soundEffects = {
  fire: new Audio("./assets/audio/1.wav"),
  explode: new Audio("./assets/audio/2.wav"),
  hit: new Audio("./assets/audio/3.wav"),
  ufoStep1: new Audio("./assets/audio/4.wav"),
  ufoStep2: new Audio("./assets/audio/5.wav"),
  ufoStep3: new Audio("./assets/audio/6.wav"),
  ufoStep4: new Audio("./assets/audio/7.wav")
}

app.ports.soundEffect.subscribe(name => soundEffects[name].play())
