import { Elm } from "./src/Main.elm"
import "./index.html"

const app = Elm.Main.init({
  node: document.getElementById("elm-node")
})

const soundEffects = {
  fire: "./assets/audio/1.wav",
  explode: "./assets/audio/2.wav",
  hit: "./assets/audio/3.wav",
  ufoStep1: "./assets/audio/4.wav",
  ufoStep2: "./assets/audio/5.wav",
  ufoStep3: "./assets/audio/6.wav",
  ufoStep4: "./assets/audio/7.wav"
}

app.ports.soundEffect.subscribe(name => new Audio(soundEffects[name]).play())
