import { autocompletion } from "@codemirror/autocomplete";
import { indentWithTab } from "@codemirror/commands";
import { oneDark } from "@codemirror/theme-one-dark";
import { keymap } from "@codemirror/view";
import { EditorView, basicSetup } from "codemirror";
import van from "vanjs-core";
import init, { why } from "ylang";
import { YLanguage } from "./lang";
import { error_to_string, value_to_string } from "./renderer";

const { div, button } = van.tags;

export function Editor() {
    window.addEventListener("keydown", (evt) => {
        if (!evt.ctrlKey || evt.key !== "s") return false;
        evt.preventDefault();
        evt.stopPropagation();
        execute();
        return true;
    });

    const editor_dom = div({ class: "editor" });
    // init editor
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
        parent: editor_dom,
    });

    // connect ylang wsam runtime
    const run_btn = button();
    run_btn.disabled = true;
    run_btn.onclick = execute;
    run_btn.innerText = "run (ctrl+s)";
    // init wasm
    init().then(() => {
        run_btn.disabled = false;
        console.log("wasm loaded");
    });

    const result_div = div({ class: "box" });

    function execute(): boolean {
        try {
            const code = editor.state.doc.toString();
            const result = why(code);

            result_div.innerHTML = value_to_string(result);
            result_div.style.display = "block";
            return true;
        } catch (err) {
            result_div.innerText = error_to_string(err);
            result_div.style.display = "block";
            result_div.classList.toggle("error");
            return false;
        }
    }

    return [editor_dom, div(run_btn), result_div];
}
