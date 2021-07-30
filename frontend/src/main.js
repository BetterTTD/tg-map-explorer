import Elm from './elm/Main.elm'

document.addEventListener("DOMContentLoaded", main);
function main() {
  const targetNode = document.getElementById("main");
  Elm.Main.init({
    node: targetNode
  });
}