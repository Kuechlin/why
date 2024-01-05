import { autocompletion } from "@codemirror/autocomplete";
import { indentWithTab } from "@codemirror/commands";
import { oneDark } from "@codemirror/theme-one-dark";
import { keymap } from "@codemirror/view";
import { EditorView, basicSetup } from "codemirror";
import init, { why } from "ylang";
import { YLanguage } from "./lang";
import { error_to_string, value_to_string } from "./renderer";

window.addEventListener("keydown", (evt) => {
    if (!evt.ctrlKey || evt.key !== "s") return false;
    evt.preventDefault();
    evt.stopPropagation();
    execute();
    return true;
});

const default_code = `let greet = fn name: string -> string {
    "hello " + name
};
greet("world")`;
const editor = new EditorView({
    doc: localStorage.getItem("code") ?? default_code,
    extensions: [
        basicSetup,
        oneDark,
        keymap.of([indentWithTab]),
        autocompletion({
            activateOnTyping: true,
        }),
        YLanguage(),
        EditorView.updateListener.of((view) => {
            if (!view.docChanged) return;
            localStorage.setItem("code", view.state.doc.toString());
        }),
    ],
    parent: document.getElementById("editor")!,
});

// connect ylang wsam runtime
const run_btn = document.getElementById("run") as HTMLButtonElement;
run_btn.disabled = true;
run_btn.onclick = execute;
// init wasm
init().then(() => {
    run_btn.disabled = false;
    console.log("wasm loaded");
});

const result_div = document.getElementById("result") as HTMLDivElement;
const error_div = document.getElementById("error") as HTMLDivElement;

function execute(): boolean {
    try {
        const code = editor.state.doc.toString();
        console.log("execute:\n" + code.trim());
        const result = why(code);

        result_div.innerHTML = value_to_string(result);
        result_div.style.display = "block";
        error_div.style.display = "none";
        return true;
    } catch (err) {
        error_div.innerText = error_to_string(err);
        result_div.style.display = "none";
        error_div.style.display = "block";
        return false;
    }
}
