import { autocompletion } from "@codemirror/autocomplete";
import { indentWithTab } from "@codemirror/commands";
import { oneDark } from "@codemirror/theme-one-dark";
import { KeyBinding, keymap } from "@codemirror/view";
import { EditorView, basicSetup } from "codemirror";
import init, { why } from "ylang";
import { YLanguage } from "./lang";
import { error_to_string, value_to_string } from "./renderer";

const saveAndRun: KeyBinding = {
    key: "c-s",
    preventDefault: true,
    stopPropagation: true,
    run: execute,
};

const editor = new EditorView({
    doc: "let a = fn a: number -> number {a*5};\n\n\n\n",
    extensions: [
        basicSetup,
        oneDark,
        keymap.of([indentWithTab, saveAndRun]),
        autocompletion({
            activateOnTyping: true,
        }),
        YLanguage(),
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
        console.log("execute:\n" + code);
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
