import van from "vanjs-core";

const { div, article, span } = van.tags;

const s = (
    key:
        | "variable"
        | "number"
        | "string"
        | "boolean"
        | "type"
        | "keyword"
        | "function"
        | "space",
    val: string
) => span({ class: key }, val);
export function Docs() {
    return div(
        { class: "docs" },
        article(
            div({ class: "title" }, "variables"),
            div(s("variable", "a"), span(" = "), s("number", "5")),
            div(s("variable", "b"), span(" = "), s("string", '"hello"')),
            div(s("variable", "c"), span(" = "), s("boolean", "true"))
        ),
        article(
            div({ class: "title" }, "compare"),
            div(s("number", "4"), span(" < "), s("number", "5")),
            div(s("number", "1"), span(" <= "), s("number", "3")),
            div(s("number", "10"), span(" > "), s("number", "2")),
            div(s("number", "8"), span(" >= "), s("number", "5")),
            div(s("string", '"a"'), span(" == "), s("string", '"a"')),
            div(s("boolean", "true"), span(" != "), s("boolean", "false"))
        ),
        article(
            div({ class: "title" }, "math"),
            div(s("number", "8"), span(" + "), s("number", "8")),
            div(s("number", "16"), span(" - "), s("number", "8")),
            div(s("number", "4"), span(" * "), s("number", "4")),
            div(s("number", "8"), span(" / "), s("number", "2"))
        ),
        article(
            div({ class: "title" }, "concat"),
            div(s("string", '"hello "'), span(" + "), s("string", '"world"')),
            div(s("string", '"val: "'), span(" + "), s("number", "8")),
            div(s("string", '"is "'), span(" + "), s("boolean", "true")),
            div(s("string", '"hello "'), span(" + "), s("variable", "name"))
        ),
        article(
            div({ class: "title" }, "functions"),
            div(
                s("variable", "add"),
                span(" = "),
                s("keyword", "fn "),
                s("variable", "a"),
                span(": "),
                s("type", "Num"),
                span(", "),
                s("variable", "b"),
                span(": "),
                s("type", "Num"),
                s("keyword", " -> "),
                s("type", "Num"),
                span(" {")
            ),
            div(
                s("space", "    "),
                s("variable", "a"),
                span(" + "),
                s("variable", "a")
            ),
            div(span("};")),
            div(
                s("function", "add"),
                span("("),
                s("number", "4"),
                span(", "),
                s("number", "8"),
                span(")")
            )
        )
    );
}
